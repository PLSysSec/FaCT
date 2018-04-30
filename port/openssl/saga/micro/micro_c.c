#include <stdint.h>

void microc(
    const uint8_t *out,
    uint32_t out_len,
    uint32_t inp_len,
    uint8_t *data,
    uint32_t num) {
  uint32_t p_res = num;
  for (uint32_t j = 0; j < out_len; j++) {
    uint32_t c = out[j];
    uint32_t mask = (uint32_t)(((int32_t)(j - inp_len)) >> (4 * 8 - 8)); // the `4` is sizeof(j)
    c &= mask;
    c |= 0x80 & (~mask) & ~((inp_len - j) >> (4 * 8 - 8)); // the `4` is sizeof(j)
    data[p_res] = (uint8_t)c;
    p_res += 1;

    if (p_res == 64) {
      p_res = 0;
    }
  }
}

uint8_t _microc(
    uint32_t j,
    uint32_t inp_len,
    uint8_t out) {

  uint32_t c = out;
  uint32_t mask = (uint32_t)(((int32_t)(j - inp_len)) >> (4 * 8 - 8)); // the `4` is sizeof(j)
  c &= mask;
  c |= 0x80 & (~mask) & ~((inp_len - j) >> (4 * 8 - 8)); // the `4` is sizeof(j)
  return (uint8_t)c;
}
