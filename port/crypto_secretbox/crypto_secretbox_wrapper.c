#include <stdint.h>
#include "crypto_secretbox.h"

int32_t crypto_secretbox(
    uint8_t c[],
    const uint8_t m[],
    uint32_t __m_len,
    const uint8_t n[24],
    const uint8_t k[32]) {
  return _crypto_secretbox(c, __m_len, m, __m_len, n, k) ? 0 : -1;
}

int32_t crypto_secretbox_open(
    uint8_t m[],
    const uint8_t c[],
    uint32_t __c_len,
    const uint8_t n[24],
    const uint8_t k[32]) {
  return _crypto_secretbox_open(m, __c_len, c, __c_len, n, k) ? 0 : -1;
}
