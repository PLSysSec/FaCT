#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

void printu8(uint8_t n) {
  printf("%02x (%u)\n", n, n);
}

void printu32(uint32_t n) {
  printf("%08x (%u)\n", n, n);
}

void printi32(int32_t n) {
  printf("%08x (%d)\n", n, n);
}

int remove_pkcs7_padding(uint8_t *buf, uint32_t public_size);

int check_good(uint32_t buflen, uint8_t padlen) {
  uint8_t * buf = malloc(buflen);
  memset(buf, 0x5c, buflen);
  for (uint8_t i = 0; i < padlen; i++) {
    buf[buflen - i - 1] = padlen;
  }

  int ret = remove_pkcs7_padding(buf, buflen);
  if (ret != buflen - padlen)
    return 1;

  for (uint32_t i = 0; i < buflen - padlen; i++) {
    if (buf[i] != 0x5c) {
      return 1;
    }
  }
  for (uint32_t i = buflen - padlen; i < buflen; i++) {
    if (buf[i] != 0x00) {
      return 1;
    }
  }
  return 0;
}

int check_good2(uint32_t buflen, uint8_t padlen) {
  uint8_t * buf = malloc(buflen);
  memset(buf, 0x5c, buflen);
  for (uint8_t i = 0; i < padlen + 2; i++) {
    buf[buflen - i - 1] = padlen;
  }

  int ret = remove_pkcs7_padding(buf, buflen);
  if (ret != buflen - padlen)
    return 1;

  for (uint32_t i = 0; i < buflen - padlen - 2; i++) {
    if (buf[i] != 0x5c) {
      return 1;
    }
  }
  for (uint32_t i = buflen - padlen - 2; i < buflen - padlen; i++) {
    if (buf[i] != padlen) {
      return 1;
    }
  }
  for (uint32_t i = buflen - padlen; i < buflen; i++) {
    if (buf[i] != 0x00) {
      return 1;
    }
  }
  return 0;
}

int check_bad1(uint32_t buflen, uint8_t padlen) {
  uint8_t * buf = malloc(buflen);
  memset(buf, 0x5c, buflen);
  for (uint8_t i = 0; i < padlen - 1; i++) {
    buf[buflen - i - 1] = padlen;
  }

  int ret = remove_pkcs7_padding(buf, buflen);
  if (ret != -1)
    return 1;
  return 0;
}

int check_bad2(uint32_t buflen, uint8_t padlen) {
  uint8_t * buf = malloc(buflen);
  memset(buf, 0x5c, buflen);
  for (uint8_t i = 0; i < padlen - 1; i++) {
    buf[buflen - i - 1] = padlen;
  }
  buf[buflen - 1] = 0xfe;

  int ret = remove_pkcs7_padding(buf, buflen);
  if (ret != -1)
    return 1;
  return 0;
}

int main() {
  int i = 0;
  if (check_good(20, 5))       goto fail;
  printf("Test %d passed\n", ++i);
  if (check_good(21, 1))       goto fail;
  printf("Test %d passed\n", ++i);
  if (check_good(22, 21))      goto fail;
  printf("Test %d passed\n", ++i);
  if (check_good2(20, 5))       goto fail;
  printf("Test %d passed\n", ++i);
  if (check_good2(21, 1))       goto fail;
  printf("Test %d passed\n", ++i);
  if (check_bad1(20, 5))       goto fail;
  printf("Test %d passed\n", ++i);
  if (check_bad2(20, 5))       goto fail;
  printf("Test %d passed\n", ++i);
  if (check_good(0x120, 0x13)) goto fail;
  printf("Test %d passed\n", ++i);
  goto ok;
fail:
  printf("\nFailed correctness test\n");
ok:
  return 0;
}
