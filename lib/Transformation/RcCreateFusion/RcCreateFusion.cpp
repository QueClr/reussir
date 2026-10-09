//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements fusion of Reussir rc.create operations.
///
//===----------------------------------------------------------------------===//

#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Alignment.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/PatternMatch.h>
#include <mlir/Interfaces/DataLayoutInterfaces.h>
#include <mlir/Pass/Pass.h>
#include <mlir/Transforms/GreedyPatternRewriteDriver.h>

namespace reussir {

#define GEN_PASS_DEF_REUSSIRRCCREATEFUSIONPASS
#include "Reussir/Transformation/Passes.h.inc"

namespace {

void eraseDeadRecordMaterialization(mlir::PatternRewriter &rewriter,
                                    mlir::Operation *op) {
  while (op && op->use_empty()) {
    mlir::Operation *next = nullptr;
    if (auto variant = llvm::dyn_cast<ReussirRecordVariantOp>(op))
      next = variant.getValue().getDefiningOp();
    rewriter.eraseOp(op);
    op = llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(next) ? next
                                                                  : nullptr;
  }
}

mlir::TypedValue<RcType> getReusedRcFromToken(mlir::Value token) {
  auto launder =
      llvm::dyn_cast_if_present<ReussirTokenLaunderOp>(token.getDefiningOp());
  if (!launder)
    return nullptr;

  auto reinterpret = llvm::dyn_cast_if_present<ReussirRcReinterpretOp>(
      launder.getToken().getDefiningOp());
  if (!reinterpret)
    return nullptr;
  return reinterpret.getRcPtr();
}

bool isLoadFromCompoundField(mlir::Value value,
                             mlir::TypedValue<RcType> sourceRc,
                             int64_t fieldIndex) {
  auto load =
      llvm::dyn_cast_if_present<ReussirRefLoadOp>(value.getDefiningOp());
  if (!load)
    return false;

  auto project = llvm::dyn_cast_if_present<ReussirRefProjectOp>(
      load.getRef().getDefiningOp());
  if (!project || project.getIndex().getSExtValue() != fieldIndex)
    return false;

  auto borrow = llvm::dyn_cast_if_present<ReussirRcBorrowOp>(
      project.getRef().getDefiningOp());
  return borrow && borrow.getRcPtr() == sourceRc;
}

// Offset of the arm payloads within a variant record: they follow the
// header (the fused 8-byte {count slot, tag} word, or the bare tag) at the
// alignment of the most aligned arm.
uint64_t getVariantPayloadOffset(RecordType variant,
                                 const mlir::DataLayout &dataLayout) {
  uint64_t headerSize = variant.hasFusedHeader()
                            ? 8
                            : dataLayout.getTypeSize(variant.getTagType());
  return llvm::alignTo(
      headerSize, variant.getElementRegionLayoutInfo(dataLayout).alignment);
}

// A reused cell already holds the new arm's field i if the old arm's field i
// has the same type and sits at the same byte offset from the start of the
// box. The arms may belong to different records, and their other members
// may differ.
bool sameVariantField(RcType sourceRc, RcType targetRc,
                      RecordType sourcePayloadType,
                      RecordType targetPayloadType, int64_t fieldIndex,
                      const mlir::DataLayout &dataLayout) {
  if (!sourcePayloadType || !targetPayloadType ||
      !sourcePayloadType.isCompound() || !targetPayloadType.isCompound())
    return false;
  if (fieldIndex < 0 ||
      static_cast<size_t>(fieldIndex) >=
          sourcePayloadType.getMembers().size() ||
      static_cast<size_t>(fieldIndex) >= targetPayloadType.getMembers().size())
    return false;
  if (sourcePayloadType.getMemberIsField()[fieldIndex] !=
          targetPayloadType.getMemberIsField()[fieldIndex] ||
      sourcePayloadType.getMembers()[fieldIndex] !=
          targetPayloadType.getMembers()[fieldIndex])
    return false;

  RcBoxType sourceBox = sourceRc.getInnerBoxType();
  RcBoxType targetBox = targetRc.getInnerBoxType();
  if (sourceBox.isHeaderFused() != targetBox.isHeaderFused() ||
      sourceBox.getHeaderTypes() != targetBox.getHeaderTypes() ||
      dataLayout.getTypeABIAlignment(sourceBox.getElementType()) !=
          dataLayout.getTypeABIAlignment(targetBox.getElementType()))
    return false;

  auto sourceVariant = llvm::dyn_cast<RecordType>(sourceBox.getElementType());
  auto targetVariant = llvm::dyn_cast<RecordType>(targetBox.getElementType());
  if (!sourceVariant || !targetVariant || !sourceVariant.isVariant() ||
      !targetVariant.isVariant())
    return false;
  if (getVariantPayloadOffset(sourceVariant, dataLayout) !=
      getVariantPayloadOffset(targetVariant, dataLayout))
    return false;

  return sourcePayloadType.getMemberOffset(dataLayout, fieldIndex) ==
         targetPayloadType.getMemberOffset(dataLayout, fieldIndex);
}

bool isLoadFromVariantField(mlir::Value value,
                            mlir::TypedValue<RcType> sourceRc,
                            mlir::TypedValue<RcType> targetRc,
                            mlir::Type targetPayloadType, int64_t fieldIndex,
                            const mlir::DataLayout &dataLayout) {
  auto load =
      llvm::dyn_cast_if_present<ReussirRefLoadOp>(value.getDefiningOp());
  if (!load)
    return false;

  auto project = llvm::dyn_cast_if_present<ReussirRefProjectOp>(
      load.getRef().getDefiningOp());
  if (!project || project.getIndex().getSExtValue() != fieldIndex)
    return false;

  auto coerce = llvm::dyn_cast_if_present<ReussirRecordCoerceOp>(
      project.getRef().getDefiningOp());
  if (!coerce)
    return false;

  auto sourcePayloadType = llvm::dyn_cast<RecordType>(
      coerce.getCoerced().getType().getElementType());
  auto targetPayloadRecord = llvm::dyn_cast<RecordType>(targetPayloadType);

  auto borrow = llvm::dyn_cast_if_present<ReussirRcBorrowOp>(
      coerce.getVariant().getDefiningOp());
  if (!borrow || borrow.getRcPtr() != sourceRc)
    return false;
  return sameVariantField(sourceRc.getType(), targetRc.getType(),
                          sourcePayloadType, targetPayloadRecord, fieldIndex,
                          dataLayout);
}

void markCompoundAvoidedCopies(ReussirRcCreateCompoundOp op) {
  auto sourceRc = op.getToken() ? getReusedRcFromToken(op.getToken())
                                : mlir::TypedValue<RcType>{};
  if (!sourceRc)
    return;
  // A load of the old record's field i is already in place only if the old
  // record is laid out like the new one. Token reuse also hands a cell of
  // one record type to another of the same size and alignment, where field
  // i can sit elsewhere, so require the same box type.
  if (sourceRc.getType().getInnerBoxType() !=
      op.getRcPtr().getType().getInnerBoxType())
    return;

  llvm::SmallVector<int64_t> skippedFields;
  for (auto [index, field] : llvm::enumerate(op.getFields()))
    if (isLoadFromCompoundField(field, sourceRc, index))
      skippedFields.push_back(static_cast<int64_t>(index));

  if (!skippedFields.empty())
    op->setAttr("skipFields",
                mlir::DenseI64ArrayAttr::get(op.getContext(), skippedFields));
}

void markVariantAvoidedCopies(ReussirRcCreateVariantOp op,
                              const mlir::DataLayout &dataLayout) {
  if (op.getValue())
    return;

  auto sourceRc = op.getToken() ? getReusedRcFromToken(op.getToken())
                                : mlir::TypedValue<RcType>{};
  if (!sourceRc)
    return;

  auto payloadType =
      op.getRecordType().getMembers()[op.getTag().getZExtValue()];
  llvm::SmallVector<int64_t> skippedFields;
  for (auto [index, field] : llvm::enumerate(op.getFields()))
    if (isLoadFromVariantField(field, sourceRc, op.getRcPtr(), payloadType,
                               index, dataLayout))
      skippedFields.push_back(static_cast<int64_t>(index));

  if (!skippedFields.empty())
    op->setAttr("skipFields",
                mlir::DenseI64ArrayAttr::get(op.getContext(), skippedFields));
}

struct FuseRcCreatePattern : public mlir::OpRewritePattern<ReussirRcCreateOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult
  matchAndRewrite(ReussirRcCreateOp create,
                  mlir::PatternRewriter &rewriter) const override {
    auto variant = llvm::dyn_cast_if_present<ReussirRecordVariantOp>(
        create.getValue().getDefiningOp());
    if (variant) {
      auto compound = llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(
          variant.getValue().getDefiningOp());
      auto fused = ReussirRcCreateVariantOp::create(
          rewriter, create.getLoc(),
          mlir::TypeRange{create.getRcPtr().getType()}, variant.getTagAttr(),
          compound ? mlir::Value{} : variant.getValue(),
          compound ? compound.getFields() : mlir::ValueRange{},
          create.getToken(), create.getRegion(), create.getVtableAttr(),
          create.getSkipRcAttr(), mlir::DenseI64ArrayAttr{},
          mlir::DenseI64ArrayAttr{});
      rewriter.replaceOp(create, fused.getRcPtr());
      eraseDeadRecordMaterialization(rewriter, variant.getOperation());
      return mlir::success();
    }

    auto compound = llvm::dyn_cast_if_present<ReussirRecordCompoundOp>(
        create.getValue().getDefiningOp());
    if (!compound)
      return mlir::failure();

    auto fused = ReussirRcCreateCompoundOp::create(
        rewriter, create.getLoc(), mlir::TypeRange{create.getRcPtr().getType()},
        compound.getFields(), create.getToken(), create.getRegion(),
        create.getVtableAttr(), create.getSkipRcAttr(),
        mlir::DenseI64ArrayAttr{}, mlir::DenseI64ArrayAttr{});
    rewriter.replaceOp(create, fused.getRcPtr());
    eraseDeadRecordMaterialization(rewriter, compound.getOperation());
    return mlir::success();
  }
};

struct RcCreateFusionPass
    : public impl::ReussirRcCreateFusionPassBase<RcCreateFusionPass> {
  using Base::Base;

  void runOnOperation() override {
    mlir::RewritePatternSet patterns(&getContext());
    patterns.add<FuseRcCreatePattern>(&getContext());
    if (mlir::failed(
            mlir::applyPatternsGreedily(getOperation(), std::move(patterns))))
      signalPassFailure();
    getOperation().walk(
        [](ReussirRcCreateCompoundOp op) { markCompoundAvoidedCopies(op); });
    mlir::DataLayout dataLayout = mlir::DataLayout::closest(getOperation());
    getOperation().walk([&](ReussirRcCreateVariantOp op) {
      markVariantAvoidedCopies(op, dataLayout);
    });
  }
};

} // namespace
} // namespace reussir
