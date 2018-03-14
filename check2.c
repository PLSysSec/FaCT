#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

int remove_pkcs7_padding(unsigned char *buf, size_t public_size);

void check_good(size_t buflen, uint8_t padlen) {
  unsigned char * buf = malloc(buflen);
  memset(buf, 0x5c, buflen);
  for (uint8_t i = 0; i < padlen; i++) {
    buf[buflen - i - 1] = padlen;
  }

  int ret = remove_pkcs7_padding(buf, buflen);
  if (ret != buflen - padlen)
    printf("Function returned %d; expected %d\n", ret, (int)(buflen - padlen));

  for (size_t i = 0; i < buflen - padlen; i++) {
    if (buf[i] == 0x00) {
      printf("Too many bytes removed!\n");
      return;
    }
    else if (buf[i] != 0x5c) {
      printf("Buffer modified incorrectly!\n");
      return;
    }
  }
  for (size_t i = buflen - padlen; i < buflen; i++) {
    if (buf[i] == padlen) {
      printf("Not enough bytes removed!\n");
      return;
    }
    else if (buf[i] != 0x00) {
      printf("Buffer modified incorrectly!\n");
      return;
    }
  }
}

void check_bad1(size_t buflen, uint8_t padlen) {
  unsigned char * buf = malloc(buflen);
  memset(buf, 0x5c, buflen);
  for (uint8_t i = 0; i < padlen - 1; i++) {
    buf[buflen - i - 1] = padlen;
  }

  int ret = remove_pkcs7_padding(buf, buflen);
  if (ret != -1)
    printf("Function returned %d; expected %d\n", ret, -1);
}

void check_bad2(size_t buflen, uint8_t padlen) {
  unsigned char * buf = malloc(buflen);
  memset(buf, 0x5c, buflen);
  for (uint8_t i = 0; i < padlen - 1; i++) {
    buf[buflen - i - 1] = padlen;
  }
  buf[buflen - 1] = 0xfe;

  int ret = remove_pkcs7_padding(buf, buflen);
  if (ret != -1)
    printf("Function returned %d; expected %d\n", ret, -1);
}

int main() {
  check_good(20, 5);
  check_good(21, 1);
  check_good(22, 21);
  check_bad1(20, 5);
  check_bad2(20, 5);
  check_good(0x120, 0x13);
  return 0;
}
