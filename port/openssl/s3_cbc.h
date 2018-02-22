#ifndef __S3_CBC_H
#define __S3_CBC_H

/* _fact_memzero is an internal function */

/* _fact_memzero64 is an internal function */

/* _memcpy is an internal function */

/* _crypto_verify_16 is an internal function */













void __ssl3_cbc_digest_record(
  /*secret*/ uint8_t md_state[216],
  /*secret*/ uint8_t mac_out[64],
  /*secret*/ uint8_t hmac_pad[128],
  /*public*/ uint64_t md_size,
  /*public*/ uint64_t md_block_size,
  /*public*/ uint64_t log_md_block_size,
  /*public*/ uint64_t sslv3_pad_length,
  /*public*/ uint64_t md_length_size,
  /*public*/ uint8_t length_is_big_endian,
  /*public*/ int32_t sha_type,
  /*secret*/ const uint8_t header[13],
  /*secret*/ const uint8_t data[],
  /*public*/ uint32_t __data_len,
  /*secret*/ uint64_t data_plus_mac_size,
  /*secret*/ const uint8_t mac_secret[],
  /*public*/ uint32_t __mac_secret_len,
  /*public*/ uint8_t is_sslv3);

#endif /* __S3_CBC_H */