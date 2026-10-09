//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for record_lift_alignment.rr: the sum over 20000 list cells.

#include <stdio.h>

extern unsigned long long reussir_main(void);

int main(void) {
  unsigned long long got = reussir_main();
  if (got != 212089998ULL) {
    fprintf(stderr, "got %llu, want 212089998\n", got);
    return 1;
  }
  return 0;
}
