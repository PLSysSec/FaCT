#include <stdint.h>

int32_t cond_sel(uint8_t cond, int32_t x, int32_t y) {
  uint32_t seltor = 0 - cond;
  return (seltor & x) | ((~seltor) & y);
}
