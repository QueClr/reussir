//===-- SCFOpsLowering.cpp - Reussir SCF ops lowering impl -----*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <format>
#include <llvm-21/llvm/Support/BLAKE3.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/MapVector.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/TypeSwitch.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/ErrorHandling.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/Math/IR/Math.h>
#include <mlir/Dialect/SCF/IR/SCF.h>
#include <mlir/Dialect/UB/IR/UBOps.h>
#include <mlir/IR/Block.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinAttributes.h>
#include <mlir/IR/ValueRange.h>
#include <mlir/Pass/Pass.h>

#include "Reussir/Conversion/SCFOpsLowering.h"
#include "Reussir/IR/ReussirDialect.h"
#include "Reussir/IR/ReussirEnumAttrs.h"
#include "Reussir/IR/ReussirOps.h"
#include "Reussir/IR/ReussirTypes.h"

namespace reussir {

#define GEN_PASS_DEF_REUSSIRSCFOPSLOWERINGPASS
#include "Reussir/Conversion/Passes.h.inc"
//===----------------------------------------------------------------------===//
// String Pattern Trie
//===----------------------------------------------------------------------===//
namespace {
struct State {
  std::optional<size_t> finalState;
  llvm::MapVector<uint8_t, size_t> children{};
};
struct Trie {
  std::vector<State> states;
};
Trie buildTrie(mlir::ArrayAttr patterns) {
  Trie trie;
  trie.states.emplace_back();
  if (!patterns)
    return trie;
  for (auto [idx, attr] : llvm::enumerate(patterns.getValue())) {
    auto strAttr = llvm::dyn_cast<mlir::StringAttr>(attr);
    if (!strAttr)
      continue;
    size_t stateIdx = 0;
    for (char c : strAttr.getValue()) {
      uint8_t key = static_cast<uint8_t>(c);
      if (trie.states[stateIdx].children.find(key) ==
          trie.states[stateIdx].children.end()) {
        trie.states[stateIdx].children[key] = trie.states.size();
        trie.states.emplace_back();
      }
      stateIdx = trie.states[stateIdx].children[key];
    }
    if (!trie.states[stateIdx].finalState.has_value())
      trie.states[stateIdx].finalState = idx;
  }
  return trie;
}
std::string b62encode(llvm::APInt value) {
  const char *alphabet =
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  if (value.isZero())
    return std::string(1, alphabet[0]);

  std::string result;
  result.reserve(43);

  while (!value.isZero()) {
    uint64_t remainder;
    llvm::APInt::udivrem(value, 62, value, remainder);
    result.push_back(alphabet[remainder]);
  }
  std::reverse(result.begin(), result.end());
  return result;
}
llvm::APInt blakeHashedPattern(mlir::ArrayAttr pattern) {
  llvm::BLAKE3 blake3;
  for (auto attr : pattern.getValue()) {
    auto strAttr = llvm::dyn_cast<mlir::StringAttr>(attr);
    if (!strAttr)
      continue;
    blake3.update(strAttr.getValue());
  }
  llvm::BLAKE3Result<32> result = blake3.final();
  auto data = std::bit_cast<std::array<uint64_t, 4>>(result);
  return llvm::APInt(256, data);
}
std::string hashPattern(mlir::ArrayAttr pattern) {
  return b62encode(blakeHashedPattern(pattern));
}
std::string decisionFunctionName(mlir::ArrayAttr patterns) {
  std::string hash = hashPattern(patterns);
  const char *sep = (hash[0] >= '0' && hash[0] <= '9') ? "_" : "";
  return std::format("_RNvC25REUSSIR_STRING_DISPATCHER{}{}{}", hash.size(), sep,
                     hash);
}
// emit decision function with signature (state, byteOffset, str) -> (index,
// i1)
// the dispatch is done with a multi-layered scf index_switch that recurses
// until a final state is reached or a failure is detected (returns 0). A
// failure happens either when the string ends prematurely or the string
// branches to a non-existing state in the trie. Hence, the dispatch body may be
// wrapped in a scf.if first to check if the length is longer than the
// byteOffset. Then, we do index switch on the state and then switch on the
// character.
std::string emitDecisionFunction(mlir::ModuleOp module,
                                 mlir::OpBuilder &builder,
                                 mlir::ArrayAttr patterns) {
  std::string name = decisionFunctionName(patterns);
  if (module.lookupSymbol(name))
    return name;

  mlir::OpBuilder::InsertionGuard guard(builder);
  builder.setInsertionPointToStart(module.getBody());
  auto indexType = builder.getIndexType();
  auto funcType = builder.getFunctionType(
      {indexType, indexType,
       reussir::StrType::get(builder.getContext(), reussir::LifeScope::local)},
      {indexType, builder.getI1Type()});
  auto func =
      builder.create<mlir::func::FuncOp>(module.getLoc(), name, funcType);
  func.setPrivate();
  func->setAttr("llvm.linkage",
                mlir::LLVM::LinkageAttr::get(builder.getContext(),
                                             mlir::LLVM::Linkage::Internal));

  mlir::Block *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto loc = module.getLoc();
  auto patternRef = patterns;
  Trie trie = buildTrie(patternRef);

  mlir::Value initialState = entryBlock->getArgument(0);
  mlir::Value initialOffset = entryBlock->getArgument(1);
  mlir::Value str = entryBlock->getArgument(2);

  auto len = builder.create<reussir::ReussirStrLenOp>(loc, indexType, str);
  auto invalidState =
      builder.create<mlir::arith::ConstantIndexOp>(loc, trie.states.size());

  auto whileOp = builder.create<mlir::scf::WhileOp>(
      loc, mlir::TypeRange{indexType, indexType},
      mlir::ValueRange{initialState, initialOffset});

  // Before Region
  {
    mlir::Block *beforeBlock = builder.createBlock(
        &whileOp.getBefore(), {}, {indexType, indexType}, {loc, loc});
    builder.setInsertionPointToStart(beforeBlock);
    auto currState = beforeBlock->getArgument(0);
    auto currOffset = beforeBlock->getArgument(1);

    auto inBounds = builder.create<mlir::arith::CmpIOp>(
        loc, mlir::arith::CmpIPredicate::ult, currOffset, len);
    auto isValidState = builder.create<mlir::arith::CmpIOp>(
        loc, mlir::arith::CmpIPredicate::ne, currState, invalidState);
    auto cond =
        builder.create<mlir::arith::AndIOp>(loc, inBounds, isValidState);
    builder.create<mlir::scf::ConditionOp>(loc, cond,
                                           beforeBlock->getArguments());
  }

  // After Region
  {
    mlir::Block *afterBlock = builder.createBlock(
        &whileOp.getAfter(), {}, {indexType, indexType}, {loc, loc});
    builder.setInsertionPointToStart(afterBlock);
    auto currState = afterBlock->getArgument(0);
    auto currOffset = afterBlock->getArgument(1);

    auto byte = builder.create<reussir::ReussirStrUnsafeByteAtOp>(
        loc, builder.getI8Type(), str, currOffset);
    auto byteIdx =
        builder.create<mlir::arith::IndexCastOp>(loc, indexType, byte);

    llvm::SmallVector<int64_t> stateCases;
    for (size_t i = 0; i < trie.states.size(); ++i) {
      stateCases.push_back(i);
    }

    auto stateSwitch = builder.create<mlir::scf::IndexSwitchOp>(
        loc, mlir::TypeRange{indexType}, currState,
        builder.getDenseI64ArrayAttr(stateCases), stateCases.size());

    // Default case for state switch
    {
      mlir::Block *defaultBlock =
          builder.createBlock(&stateSwitch.getDefaultRegion());
      builder.setInsertionPointToStart(defaultBlock);
      builder.create<mlir::scf::YieldOp>(loc, invalidState->getResults());
    }

    // Cases for each state
    for (size_t i = 0; i < trie.states.size(); ++i) {
      mlir::Block *caseBlock =
          builder.createBlock(&stateSwitch.getCaseRegions()[i]);
      builder.setInsertionPointToStart(caseBlock);

      const auto &state = trie.states[i];
      llvm::SmallVector<int64_t> charCases;
      for (auto const &[key, _] : state.children) {
        charCases.push_back(key);
      }

      auto byteSwitch = builder.create<mlir::scf::IndexSwitchOp>(
          loc, mlir::TypeRange{indexType}, byteIdx,
          builder.getDenseI64ArrayAttr(charCases), charCases.size());

      // Byte Default: invalid state
      {
        mlir::Block *byteDefault =
            builder.createBlock(&byteSwitch.getDefaultRegion());
        builder.setInsertionPointToStart(byteDefault);
        builder.create<mlir::scf::YieldOp>(loc, invalidState->getResults());
      }

      // Byte Cases
      for (size_t j = 0; j < charCases.size(); ++j) {
        uint8_t key = static_cast<uint8_t>(charCases[j]);
        size_t nextStateIdx = state.children.find(key)->second;
        mlir::Block *byteCase =
            builder.createBlock(&byteSwitch.getCaseRegions()[j]);
        builder.setInsertionPointToStart(byteCase);
        auto nextStateVal =
            builder.create<mlir::arith::ConstantIndexOp>(loc, nextStateIdx);
        builder.create<mlir::scf::YieldOp>(loc, nextStateVal->getResults());
      }

      builder.setInsertionPointAfter(byteSwitch);
      builder.create<mlir::scf::YieldOp>(loc, byteSwitch.getResult(0));
    }

    builder.setInsertionPointAfter(stateSwitch);
    auto nextOffset = builder.create<mlir::arith::AddIOp>(
        loc, currOffset, builder.create<mlir::arith::ConstantIndexOp>(loc, 1));
    builder.create<mlir::scf::YieldOp>(
        loc, mlir::ValueRange{stateSwitch.getResult(0), nextOffset});
  }

  mlir::Value finalState = whileOp.getResult(0);
  builder.setInsertionPointAfter(whileOp);

  // Final decision based on state
  llvm::SmallVector<int64_t> resultCases;
  for (size_t i = 0; i < trie.states.size(); ++i)
    resultCases.push_back(i);

  auto trueVal = builder.create<mlir::arith::ConstantIntOp>(loc, 1, 1);
  auto falseVal = builder.create<mlir::arith::ConstantIntOp>(loc, 0, 1);
  auto poison = builder.create<mlir::ub::PoisonOp>(loc, indexType);

  auto resultSwitch = builder.create<mlir::scf::IndexSwitchOp>(
      loc, mlir::TypeRange{indexType, builder.getI1Type()}, finalState,
      builder.getDenseI64ArrayAttr(resultCases), resultCases.size());

  // Default result: {poison, false}
  {
    mlir::Block *defaultBlock =
        builder.createBlock(&resultSwitch.getDefaultRegion());
    builder.setInsertionPointToStart(defaultBlock);
    builder.create<mlir::scf::YieldOp>(loc, mlir::ValueRange{poison, falseVal});
  }

  for (size_t i = 0; i < trie.states.size(); ++i) {
    mlir::Block *caseBlock =
        builder.createBlock(&resultSwitch.getCaseRegions()[i]);
    builder.setInsertionPointToStart(caseBlock);
    if (trie.states[i].finalState.has_value()) {
      auto idx = builder.create<mlir::arith::ConstantIndexOp>(
          loc, *trie.states[i].finalState);
      builder.create<mlir::scf::YieldOp>(loc, mlir::ValueRange{idx, trueVal});
    } else {
      builder.create<mlir::scf::YieldOp>(loc,
                                         mlir::ValueRange{poison, falseVal});
    }
  }

  builder.setInsertionPointToEnd(entryBlock);
  builder.create<mlir::func::ReturnOp>(loc, resultSwitch.getResults());
  return name;
}
} // namespace

//===----------------------------------------------------------------------===//
// Conversion patterns
//===----------------------------------------------------------------------===//

namespace {

struct ReussirNullableDispatchOpRewritePattern
    : public mlir::OpRewritePattern<ReussirNullableDispatchOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirNullableDispatchOp op,
                  mlir::PatternRewriter &rewriter) const override {
    // First, create a check operation to get the null flag from the input.
    auto flag = rewriter.create<reussir::ReussirNullableCheckOp>(
        op.getLoc(), op.getNullable());
    auto scfIfOp = rewriter.create<mlir::scf::IfOp>(
        op.getLoc(), op->getResultTypes(), flag, /*addThenRegion=*/true,
        /*addElseRegion=*/true);
    // first, do the easy part, for else region, we can just inline the
    // operation
    rewriter.inlineBlockBefore(&*op.getNullRegion().begin(),
                               &*scfIfOp.getElseRegion().begin(),
                               scfIfOp.getElseRegion().begin()->begin());

    // Now, for the then region, we first create the coerced value
    rewriter.setInsertionPointToStart(&scfIfOp.getThenRegion().front());
    auto coerced = rewriter.create<reussir::ReussirNullableCoerceOp>(
        op.getLoc(), op.getNullable().getType().getPtrTy(), op.getNullable());
    // Then we inline the region, supplying coerced value as the argument
    rewriter.inlineBlockBefore(
        &*op.getNonNullRegion().begin(), &*scfIfOp.getThenRegion().begin(),
        scfIfOp.getThenRegion().begin()->end(), mlir::ValueRange{coerced});
    rewriter.replaceOp(op, scfIfOp);
    return mlir::success();
  }
};

struct ReussirRecordDispatchOpRewritePattern
    : public mlir::OpRewritePattern<ReussirRecordDispatchOp> {
  using OpRewritePattern::OpRewritePattern;

private:
  static mlir::DenseI64ArrayAttr
  getAllTagsAsSingletons(ReussirRecordDispatchOp op) {
    llvm::SmallVector<int64_t> allTags;
    for (auto tagSet : op.getTagSets()) {
      mlir::DenseI64ArrayAttr tagArray =
          llvm::cast<mlir::DenseI64ArrayAttr>(tagSet);
      if (tagArray.size() != 1)
        return {};
      allTags.push_back(tagArray[0]);
    }
    return mlir::DenseI64ArrayAttr::get(op.getContext(), allTags);
  }
  static mlir::Value buildPreDispatcher(ReussirRecordTagOp tag,
                                        ReussirRecordDispatchOp op,
                                        mlir::PatternRewriter &rewriter) {
    mlir::OpBuilder::InsertionGuard guard(rewriter);
    llvm::DenseMap<int64_t, int64_t> tagToRegionIdx;
    llvm::SmallVector<int64_t> allTags;
    for (auto [idx, tagSet] : llvm::enumerate(op.getTagSets())) {
      mlir::DenseI64ArrayAttr tagArray =
          llvm::cast<mlir::DenseI64ArrayAttr>(tagSet);
      for (auto tag : tagArray.asArrayRef()) {
        allTags.push_back(tag);
        tagToRegionIdx[tag] = idx;
      }
    };
    auto indexSwitchOp = rewriter.create<mlir::scf::IndexSwitchOp>(
        op.getLoc(), rewriter.getIndexType(), tag.getResult(), allTags,
        allTags.size());

    for (auto [tag, region] :
         llvm::zip(allTags, indexSwitchOp.getCaseRegions())) {
      mlir::Block *block = rewriter.createBlock(&region, region.begin());
      rewriter.setInsertionPointToStart(block);
      auto constantIdx = rewriter.create<mlir::arith::ConstantIndexOp>(
          op.getLoc(), tagToRegionIdx[tag]);
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(),
                                          constantIdx->getResults());
    }
    {
      mlir::Block *block =
          rewriter.createBlock(&indexSwitchOp.getDefaultRegion(),
                               indexSwitchOp.getDefaultRegion().begin());
      rewriter.setInsertionPointToStart(block);
      auto poison = rewriter.create<mlir::ub::PoisonOp>(
          op.getLoc(), rewriter.getIndexType());
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(), poison->getResults());
    }
    return indexSwitchOp.getResult(0);
  }

public:
  mlir::LogicalResult
  matchAndRewrite(ReussirRecordDispatchOp op,
                  mlir::PatternRewriter &rewriter) const override {
    // First, create a RecordTagOps operation to get the tag from the input.
    auto tag = rewriter.create<reussir::ReussirRecordTagOp>(op.getLoc(),
                                                            op.getVariant());

    mlir::Value outerSwitchValue;
    mlir::DenseI64ArrayAttr outerSwitchCases;

    if (auto allTags = getAllTagsAsSingletons(op)) {
      outerSwitchValue = tag.getResult();
      outerSwitchCases = allTags;
    } else {
      outerSwitchValue = buildPreDispatcher(tag, op, rewriter);
      outerSwitchCases = mlir::DenseI64ArrayAttr::get(
          op.getContext(),
          llvm::to_vector(llvm::seq<int64_t>(0, op.getTagSets().size())));
    }

    auto indexSwitchOp = rewriter.create<mlir::scf::IndexSwitchOp>(
        op.getLoc(), op->getResultTypes(), outerSwitchValue, outerSwitchCases,
        outerSwitchCases.size());
    // mark default region as unreachable
    {
      mlir::Block *block =
          rewriter.createBlock(&indexSwitchOp.getDefaultRegion(),
                               indexSwitchOp.getDefaultRegion().begin());
      rewriter.setInsertionPointToStart(block);
      llvm::SmallVector<mlir::Value, 1> poisonValues;
      if (op.getValue()) {
        auto poison = rewriter.create<mlir::ub::PoisonOp>(
            op.getLoc(), op.getValue().getType());
        poisonValues.push_back(poison);
      }
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(), poisonValues);
    }
    for (auto [idx, tagSet, region] :
         llvm::enumerate(op.getTagSets(), indexSwitchOp.getCaseRegions())) {
      mlir::DenseI64ArrayAttr tagArray =
          llvm::cast<mlir::DenseI64ArrayAttr>(tagSet);
      llvm::SmallVector<mlir::Value, 1> args;
      mlir::Block *block = rewriter.createBlock(&region, region.begin());
      rewriter.setInsertionPointToStart(block);
      // if we know exact variant, we need to coerce the variant to the exact
      // type
      if (tagArray.size() == 1) {
        RefType variantRef = op.getVariant().getType();
        RecordType recordType =
            llvm::cast<RecordType>(variantRef.getElementType());
        mlir::Type targetVariantType =
            getProjectedType(recordType.getMembers()[tagArray[0]],
                             recordType.getMemberIsField()[tagArray[0]],
                             variantRef.getCapability());
        RefType coercedType =
            RefType::get(rewriter.getContext(), targetVariantType,
                         variantRef.getCapability());
        auto coerced = rewriter.create<reussir::ReussirRecordCoerceOp>(
            op.getLoc(), coercedType, rewriter.getIndexAttr(tagArray[0]),
            op.getVariant());
        args.push_back(coerced);
      }
      // inline the block, supplying coerced value as the argument
      rewriter.inlineBlockBefore(&op->getRegion(idx).front(), block,
                                 block->end(), args);
    }
    rewriter.replaceOp(op, indexSwitchOp);
    return mlir::success();
  }
};

struct ReussirClosureUniqifyOpRewritePattern
    : public mlir::OpRewritePattern<ReussirClosureUniqifyOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirClosureUniqifyOp op,
                  mlir::PatternRewriter &rewriter) const override {
    // Create a check operation to see if the closure is unique
    auto isUnique = rewriter.create<reussir::ReussirRcIsUniqueOp>(
        op.getLoc(), op.getClosure());

    // Create an SCF if-else operation
    auto scfIfOp = rewriter.create<mlir::scf::IfOp>(
        op.getLoc(), op->getResultTypes(), isUnique, /*addThenRegion=*/true,
        /*addElseRegion=*/true);

    // In the then region (closure is unique), just return the original closure
    rewriter.setInsertionPointToStart(&scfIfOp.getThenRegion().front());
    rewriter.create<mlir::scf::YieldOp>(op.getLoc(), op.getClosure());

    // In the else region (closure is not unique), clone the closure, dec the
    // original rc pointer
    rewriter.setInsertionPointToStart(&scfIfOp.getElseRegion().front());
    auto cloned = rewriter.create<reussir::ReussirClosureCloneOp>(
        op.getLoc(), op.getClosure().getType(), op.getClosure());
    rewriter.create<reussir::ReussirRcDecOp>(op.getLoc(), mlir::Type{},
                                             op.getClosure());
    rewriter.create<mlir::scf::YieldOp>(op.getLoc(), cloned.getResult());

    rewriter.replaceOp(op, scfIfOp);
    return mlir::success();
  }
};

struct ReussirScfYieldOpRewritePattern
    : public mlir::OpRewritePattern<ReussirScfYieldOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirScfYieldOp op,
                  mlir::PatternRewriter &rewriter) const override {
    rewriter.replaceOpWithNewOp<mlir::scf::YieldOp>(op, op->getOperands());
    return mlir::success();
  }
};

struct ReussirTokenEnsureOpRewritePattern
    : public mlir::OpRewritePattern<ReussirTokenEnsureOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirTokenEnsureOp op,
                  mlir::PatternRewriter &rewriter) const override {
    auto nullableDispatchOp = rewriter.create<ReussirNullableDispatchOp>(
        op.getLoc(), op.getType(), op.getNullableToken());

    {
      mlir::Block *thenBlock =
          rewriter.createBlock(&nullableDispatchOp.getNonNullRegion(), {},
                               op.getType(), {op.getLoc()});
      rewriter.setInsertionPointToStart(thenBlock);
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(),
                                          thenBlock->getArgument(0));
    }
    {
      mlir::Block *elseBlock =
          rewriter.createBlock(&nullableDispatchOp.getNullRegion());
      rewriter.setInsertionPointToStart(elseBlock);
      auto allocatedToken =
          rewriter.create<ReussirTokenAllocOp>(op.getLoc(), op.getType());
      rewriter.create<mlir::scf::YieldOp>(op.getLoc(),
                                          allocatedToken->getResults());
    }
    rewriter.replaceOp(op, nullableDispatchOp);
    return mlir::success();
  }
};

struct ReussirStrByteAtOpRewritePattern
    : public mlir::OpRewritePattern<ReussirStrByteAtOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirStrByteAtOp op,
                  mlir::PatternRewriter &rewriter) const override {
    mlir::Location loc = op.getLoc();

    // Get string length
    auto lenOp = rewriter.create<reussir::ReussirStrLenOp>(
        loc, rewriter.getIndexType(), op.getStr());

    // Check if index is within bounds (index < len)
    auto inBounds = rewriter.create<mlir::arith::CmpIOp>(
        loc, mlir::arith::CmpIPredicate::ult, op.getIndex(), lenOp.getResult());

    // Create if-else block
    auto ifOp = rewriter.create<mlir::scf::IfOp>(
        loc, op.getResult().getType(), inBounds, /*addThenRegion=*/true,
        /*addElseRegion=*/true);

    // Then region: Unsafe access
    {
      auto &thenBlock = ifOp.getThenRegion().front();
      rewriter.setInsertionPointToStart(&thenBlock);
      auto unsafeByte = rewriter.create<reussir::ReussirStrUnsafeByteAtOp>(
          loc, rewriter.getI8Type(), op.getStr(), op.getIndex());
      rewriter.create<mlir::scf::YieldOp>(loc, unsafeByte.getResult());
    }

    // Else region: Return 0
    {
      auto &elseBlock = ifOp.getElseRegion().front();
      rewriter.setInsertionPointToStart(&elseBlock);
      auto zero = rewriter.create<mlir::arith::ConstantIntOp>(loc, 0, 8);
      rewriter.create<mlir::scf::YieldOp>(loc, zero->getResult(0));
    }

    rewriter.replaceOp(op, ifOp.getResult(0));
    return mlir::success();
  }
};
struct ReussirStrSelectOpRewritePattern
    : public mlir::OpRewritePattern<ReussirStrSelectOp> {
  using OpRewritePattern::OpRewritePattern;
  mlir::LogicalResult
  matchAndRewrite(ReussirStrSelectOp op,
                  mlir::PatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto funcName = emitDecisionFunction(module, rewriter, op.getPatterns());

    auto zero = rewriter.create<mlir::arith::ConstantIndexOp>(op.getLoc(), 0);
    auto func = module.lookupSymbol<mlir::func::FuncOp>(funcName);
    auto call = rewriter.create<mlir::func::CallOp>(
        op.getLoc(), func, mlir::ValueRange{zero, zero, op.getStr()});

    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};
} // namespace

//===----------------------------------------------------------------------===//
// SCFOpsLoweringPass
//===----------------------------------------------------------------------===//

namespace {
struct SCFOpsLoweringPass
    : public impl::ReussirSCFOpsLoweringPassBase<SCFOpsLoweringPass> {
  using Base::Base;
  void runOnOperation() override {
    mlir::ConversionTarget target(getContext());
    mlir::RewritePatternSet patterns(&getContext());

    populateSCFOpsLoweringConversionPatterns(patterns);

    // Configure target legality
    target.addLegalDialect<mlir::arith::ArithDialect, mlir::scf::SCFDialect,
                           mlir::math::MathDialect, mlir::func::FuncDialect,
                           mlir::ub::UBDialect, reussir::ReussirDialect>();

    // Illegal operations
    target.addIllegalOp<ReussirNullableDispatchOp, ReussirRecordDispatchOp,
                        ReussirScfYieldOp, ReussirClosureUniqifyOp,
                        ReussirTokenEnsureOp, ReussirStrByteAtOp,
                        ReussirStrSelectOp>();

    if (failed(applyPartialConversion(getOperation(), target,
                                      std::move(patterns))))
      signalPassFailure();
  }
};
} // namespace

void populateSCFOpsLoweringConversionPatterns(
    mlir::RewritePatternSet &patterns) {
  // Add conversion patterns for Reussir SCF operations
  patterns
      .add<ReussirNullableDispatchOpRewritePattern,
           ReussirRecordDispatchOpRewritePattern,
           ReussirClosureUniqifyOpRewritePattern,
           ReussirScfYieldOpRewritePattern, ReussirTokenEnsureOpRewritePattern,
           ReussirStrByteAtOpRewritePattern, ReussirStrSelectOpRewritePattern>(
          patterns.getContext());
}

} // namespace reussir
