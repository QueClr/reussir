//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for nullable_match_enum_drop.rr.

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern uint64_t null_drops_ffi(uint64_t);
extern uint64_t nonnull_keeps_ffi(uint64_t);
extern uint64_t flipped_null_ffi(uint64_t);
extern uint64_t flipped_nonnull_ffi(uint64_t);
extern uint64_t scalar_null_ffi(uint64_t);
extern uint64_t scalar_nonnull_ffi(uint64_t);

#define EXPECT(what, got, want)                                                \
  do {                                                                         \
    unsigned long long g = (got), w = (want);                                  \
    if (g != w) {                                                              \
      fprintf(stderr, "FAIL %s: got %llu want %llu\n", what, g, w);            \
      abort();                                                                 \
    }                                                                          \
  } while (0)

int main(void) {
  EXPECT("null_drops", null_drops_ffi(5), 1);           /* T::L */
  EXPECT("nonnull_keeps", nonnull_keeps_ffi(5), 15);    /* head of x */
  EXPECT("flipped_null", flipped_null_ffi(5), 5);       /* head of x */
  EXPECT("flipped_nonnull", flipped_nonnull_ffi(5), 5); /* b.v */
  EXPECT("scalar_null", scalar_null_ffi(5), 0);
  EXPECT("scalar_nonnull", scalar_nonnull_ffi(5), 15); /* head of x */
  return 0;
}
