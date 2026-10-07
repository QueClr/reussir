// Driver for reuse_launder_loop.rr: the result must be A{250, 9648}, that
// is 250 * 100000 + 9648.

#include <stdint.h>
#include <stdio.h>

extern uint64_t rv_ffi(uint64_t q);

int main(void) {
  uint64_t got = rv_ffi(0);
  if (got != 25009648u) {
    fprintf(stderr, "reuse_launder_loop: got %llu, want 25009648\n",
            (unsigned long long)got);
    return 1;
  }
  return 0;
}
