#ifndef __CRYPTO_POLY1305_H
#define __CRYPTO_POLY1305_H

void fact_memzero(
  /*secret*/ uint8_t buf[],
  /*public*/ uint32_t __buf_len,
  /*secret*/ uint8_t __fctx);

void fact_memzero64(
  /*secret*/ uint64_t buf[],
  /*public*/ uint32_t __buf_len,
  /*secret*/ uint8_t __fctx);

/*secret*/
uint32_t rotl32(
  /*secret*/ uint32_t x,
  /*public*/ int32_t b);

/*secret*/
uint64_t load64_le(
  /*secret*/ const uint8_t src[8]);

/*secret*/
uint32_t load32_le(
  /*secret*/ const uint8_t src[4]);

void store64_le(
  /*secret*/ uint8_t dst[8],
  /*secret*/ uint64_t w,
  /*secret*/ uint8_t __fctx);

void store32_le(
  /*secret*/ uint8_t dst[4],
  /*secret*/ uint32_t w,
  /*secret*/ uint8_t __fctx);

void poly1305_blocks(
  /*secret*/ uint64_t state_r[3],
  /*secret*/ uint64_t state_h[3],
  /*secret*/ uint64_t state_pad[2],
  /*public*/ uint64_t* state_leftover,
  /*secret*/ uint8_t state_buffer[16],
  /*secret*/ uint8_t* state_final,
  /*secret*/ const uint8_t m[],
  /*public*/ uint32_t __m_len,
  /*secret*/ uint8_t __fctx);

void poly1305_init(
  /*secret*/ uint64_t state_r[3],
  /*secret*/ uint64_t state_h[3],
  /*secret*/ uint64_t state_pad[2],
  /*public*/ uint64_t* state_leftover,
  /*secret*/ uint8_t state_buffer[16],
  /*secret*/ uint8_t* state_final,
  /*secret*/ const uint8_t key[32],
  /*secret*/ uint8_t __fctx);

void poly1305_update(
  /*secret*/ uint64_t state_r[3],
  /*secret*/ uint64_t state_h[3],
  /*secret*/ uint64_t state_pad[2],
  /*public*/ uint64_t* state_leftover,
  /*secret*/ uint8_t state_buffer[16],
  /*secret*/ uint8_t* state_final,
  /*secret*/ const uint8_t m[],
  /*public*/ uint32_t __m_len,
  /*secret*/ uint8_t __fctx);

void poly1305_finish(
  /*secret*/ uint64_t state_r[3],
  /*secret*/ uint64_t state_h[3],
  /*secret*/ uint64_t state_pad[2],
  /*public*/ uint64_t* state_leftover,
  /*secret*/ uint8_t state_buffer[16],
  /*secret*/ uint8_t* state_final,
  /*public*/ uint8_t mac[16],
  /*secret*/ uint8_t __fctx);

/*public*/
int32_t crypto_onetimeauth_poly1305(
  /*public*/ uint8_t out[16],
  /*secret*/ const uint8_t m[],
  /*public*/ uint32_t __m_len,
  /*secret*/ const uint8_t key[32],
  /*secret*/ uint8_t __fctx);

#endif /* __CRYPTO_POLY1305_H */