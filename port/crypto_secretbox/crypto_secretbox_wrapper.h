/*public*/
int32_t crypto_secretbox(
  /*public*/ uint8_t c[],
  /*secret*/ const uint8_t m[],
  /*public*/ uint32_t __m_len,
  /*public*/ const uint8_t n[16],
  /*secret*/ const uint8_t k[32]);

