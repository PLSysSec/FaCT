/*public*/
int32_t crypto_secretbox(
  /*public*/ uint8_t c[],
  /*secret*/ const uint8_t m[],
  /*public*/ uint32_t __m_len,
  /*public*/ const uint8_t n[24],
  /*secret*/ const uint8_t k[32]);

/*public*/
uint8_t crypto_secretbox_open(
  /*secret*/ uint8_t m[],
  /*secret*/ const uint8_t c[],
  /*public*/ uint32_t __c_len,
  /*public*/ const uint8_t n[24],
  /*secret*/ const uint8_t k[32]);
