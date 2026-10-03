//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for unique_carrying_shared_field.rr.

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern uint64_t unique_carrying_shared_field(uint64_t n);

int main(void) {
  uint64_t result = unique_carrying_shared_field(3);
  if (result != 101001) {
    fprintf(stderr, "FAIL: expected 101001, got %llu\n",
            (unsigned long long)result);
    abort();
  }
  return 0;
}
