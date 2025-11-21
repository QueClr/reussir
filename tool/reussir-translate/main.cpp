//===-- main.cpp - Reussir translation driver main --------------*- C++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//

#include <mlir/IR/DialectRegistry.h>
#include <mlir/InitAllDialects.h>
#include <mlir/InitAllExtensions.h>
#include <mlir/InitAllTranslations.h>
#include <mlir/Support/LogicalResult.h>
#include <mlir/Target/LLVMIR/Dialect/All.h>
#include <mlir/Tools/mlir-translate/MlirTranslateMain.h>
#include <mlir/Tools/mlir-translate/Translation.h>

#include "Reussir/IR/ReussirDialect.h"

int main(int argc, char **argv) {
  mlir::registerAllTranslations();
  mlir::TranslateFromMLIRRegistration registration(
      "reussir-to-llvmir", "Translate MLIR to LLVMIR",
      [](mlir::Operation *op, llvm::raw_ostream &output) {
        llvm::LLVMContext llvmContext;
        auto llvmModule = translateModuleToLLVMIR(op, llvmContext);
        if (!llvmModule)
          return mlir::failure();

        llvmModule->removeDebugIntrinsicDeclarations();
        llvmModule->print(output, nullptr);
        return mlir::success();
      },
      [](mlir::DialectRegistry &registry) {
        registry.insert<mlir::DLTIDialect, mlir::func::FuncDialect>();
        reussir::registerReussirDialectTranslation(registry);
        mlir::registerAllToLLVMIRTranslations(registry);
      });
  return failed(
      mlir::mlirTranslateMain(argc, argv, "Reussir translation driver\n"));
}