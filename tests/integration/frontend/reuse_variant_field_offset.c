//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for reuse_variant_field_offset.rr.

#include <stdint.h>
#include <stdio.h>

extern uint64_t moved_field(uint64_t c, uint64_t x);
extern uint64_t widened_member(uint64_t x, uint64_t a, uint64_t y);
extern uint64_t self_widened(uint64_t x, uint64_t a);

int main(void) {
  uint64_t moved = moved_field(5, 11);
  uint64_t widened = widened_member(1, 2, 777);
  uint64_t self = self_widened(1, 2);
  if (moved != 5001 || widened != 9008777 || self != 1020304) {
    fprintf(stderr, "got %llu, %llu and %llu, want 5001, 9008777 and 1020304\n",
            (unsigned long long)moved, (unsigned long long)widened,
            (unsigned long long)self);
    return 1;
  }
  return 0;
}
