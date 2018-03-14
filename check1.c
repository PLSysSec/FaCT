#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void remove_secret_padding(unsigned char *p, size_t public_size, size_t secret_len);

void check(size_t secret_len, size_t public_size) {
  unsigned char * p = malloc(public_size);
  memset(p, 0x5c, public_size);
  remove_secret_padding(p, public_size, secret_len);
  for (size_t i = 0; i < secret_len; i++) {
    if (p[i] == 0x00) {
      printf("Too many bytes removed!\n");
      return;
    }
    else if (p[i] != 0x5c) {
      printf("Buffer modified incorrectly!\n");
      return;
    }
  }
  for (size_t i = secret_len; i < public_size; i++) {
    if (p[i] == 0x5c) {
      printf("Not enough bytes removed!\n");
      return;
    }
    else if (p[i] != 0x00) {
      printf("Buffer modified incorrectly!\n");
      return;
    }
  }
}

int main() {
  check(2, 5);
  check(14, 32);
  check(11, 25);
  check(0, 23);
  check(19, 19);
  return 0;
}
