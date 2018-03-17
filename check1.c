#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void remove_secret_padding(unsigned char *p, size_t public_size, size_t secret_len);

int check(size_t secret_len, size_t public_size) {
  unsigned char * p = malloc(public_size);
  memset(p, 0x5c, public_size);
  remove_secret_padding(p, public_size, secret_len);
  for (size_t i = 0; i < secret_len; i++) {
    if (p[i] == 0x00) {
      return 1;
    }
    else if (p[i] != 0x5c) {
      return 1;
    }
  }
  for (size_t i = secret_len; i < public_size; i++) {
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
  if (check(2, 5))   goto fail;
  if (check(14, 32)) goto fail;
  if (check(11, 25)) goto fail;
  if (check(0, 23))  goto fail;
  if (check(19, 19)) goto fail;
  goto ok;
fail:
  printf("Failed correctness test\n");
ok:
  return 0;
}
