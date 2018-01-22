#ifndef __S3_CBC_H
#define __S3_CBC_H

void _fact_memzero(
  /*secret*/ uint8_t buf[],
  /*public*/ uint32_t __buf_len);

void _fact_memzero64(
  /*secret*/ uint64_t buf[],
  /*public*/ uint32_t __buf_len);

void _memcpy(
  /*secret*/ uint8_t dst[],
  /*public*/ uint32_t __dst_len,
  /*secret*/ const uint8_t src[],
  /*public*/ uint32_t __src_len);

/*secret*/
uint8_t _crypto_verify_16(
  /*secret*/ const uint8_t x[],
  /*secret*/ const uint8_t y[]);

/*secret*/
uint32_t _rotl32(
  /*secret*/ uint32_t x,
  /*public*/ int32_t b);

/*secret*/
uint64_t _load64_le(
  /*secret*/ const uint8_t src[]);

/*secret*/
uint32_t _load32_le(
  /*secret*/ const uint8_t src[]);

void _store64_le(
  /*secret*/ uint8_t dst[],
  /*secret*/ uint64_t w);

void _store32_le(
  /*secret*/ uint8_t dst[],
  /*secret*/ uint32_t w);





void ssl3_cbc_digest_record(
  /*public*/ uint64_t md_size,
  /*public*/ uint64_t md_block_size,
  /*public*/ uint64_t log_md_block_size,
  /*public*/ uint64_t sslv3_pad_length,
  /*public*/ uint64_t md_length_size,
  /*public*/ uint8_t length_is_big_endian,
  /*secret*/ const uint8_t header[],
  /*secret*/ const uint8_t data[],
  /*public*/ uint32_t __data_len,
  /*secret*/ uint64_t data_plus_mac_size,
  /*secret*/ const uint8_t mac_secret[],
  /*public*/ uint32_t __mac_secret_len,
  /*public*/ uint8_t is_sslv3);

#endif /* __S3_CBC_H */