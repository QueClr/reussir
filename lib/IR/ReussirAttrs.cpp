//===-- ReussirAttrs.cpp - Reussir attributes implementation ----*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements the attributes used in the Reussir dialect.
//
//===----------------------------------------------------------------------===//
#include "Reussir/IR/ReussirAttrs.h"
#include "Reussir/IR/ReussirDialect.h"

#include <llvm/ADT/TypeSwitch.h>
#include <mlir/IR/Attributes.h>
#include <mlir/IR/DialectImplementation.h>
#include <mlir/Parser/Parser.h>

#define GET_ATTRDEF_CLASSES
#include "Reussir/IR/ReussirAttrs.cpp.inc"

namespace reussir {

//===----------------------------------------------------------------------===//
// ReussirDialect Attributes Registration
//===----------------------------------------------------------------------===//

mlir::Attribute ReussirDialect::parseAttribute(mlir::DialectAsmParser &parser,
                                               mlir::Type type) const {
  llvm::SMLoc typeLoc = parser.getCurrentLocation();
  llvm::StringRef mnemonic;
  mlir::Attribute genAttr;
  mlir::OptionalParseResult parseResult =
      generatedAttributeParser(parser, &mnemonic, type, genAttr);
  if (parseResult.has_value())
    return genAttr;
  parser.emitError(typeLoc, "unknown attribute in Reussir dialect");
  return {};
}

void ReussirDialect::printAttribute(mlir::Attribute attr,
                                    mlir::DialectAsmPrinter &os) const {
  if (failed(generatedAttributePrinter(attr, os)))
    llvm_unreachable("unexpected Reussir attribute kind");
}

void ReussirDialect::registerAttributes() {
  addAttributes<
#define GET_ATTRDEF_LIST
#include "Reussir/IR/ReussirAttrs.cpp.inc"
      >();
}
} // namespace reussir
