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

void remove_secret_padding(uint8_t *p, uint32_t public_size, uint32_t secret_len);

int check(uint32_t secret_len, uint32_t public_size) {
  uint8_t * p = malloc(public_size);
  memset(p, 0x5c, public_size);
  remove_secret_padding(p, public_size, secret_len);
  for (uint32_t i = 0; i < secret_len; i++) {
    if (p[i] == 0x00) {
      return 1;
    }
    else if (p[i] != 0x5c) {
      return 1;
    }
  }
  for (uint32_t i = secret_len; i < public_size; i++) {
    if (p[i] == 0x5c) {
      return 1;
    }
    else if (p[i] != 0x00) {
      return 1;
    }
  }
  return 0;
}

int main() {
  int i = 0;
  if (check(2, 5))   goto fail;
  printf("Test %d passed\n", ++i);
  if (check(14, 32)) goto fail;
  printf("Test %d passed\n", ++i);
  if (check(11, 25)) goto fail;
  printf("Test %d passed\n", ++i);
  if (check(0, 23))  goto fail;
  printf("Test %d passed\n", ++i);
  if (check(19, 19)) goto fail;
  printf("Test %d passed\n", ++i);
  goto ok;
fail:
  printf("\nFailed correctness test\n");
ok:
  return 0;
}
