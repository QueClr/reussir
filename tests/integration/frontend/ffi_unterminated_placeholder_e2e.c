//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for ffi_unterminated_placeholder_e2e.rr: 4 * 100 + 43.

extern unsigned long long reussir_main(void);

int main(void) { return reussir_main() == 443 ? 0 : 1; }
