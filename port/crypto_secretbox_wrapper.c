#include <stdint.h>
#include "crypto_secretbox.h"

int32_t crypto_secretbox(
    uint8_t c[],
    const uint8_t m[],
    uint32_t __m_len,
    const uint8_t n[16],
    const uint8_t k[32]) {
  return _crypto_secretbox(c, __m_len, m, __m_len, n, k, 1);
}
