//===-- JITEngine.cppm - Reussir JIT engine ---------------------*- c++ -*-===//
//
// Part of the Reussir project, dual licensed under the Apache License v2.0 or
// the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
//
// This file implements the JIT engine for Reussir.
//===----------------------------------------------------------------------===//
module;
#include "Reussir/Bridge.h"
export module Reussir.JITEngine;
import Reussir.Bridge;
namespace reussir {
void test() { bridge::setSpdlogLevel(REUSSIR_LOG_INFO); }
} // namespace reussir
