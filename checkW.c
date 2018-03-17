#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int add5(int num);

int check(int num) {
  if (num+5 == add5(num)) {
    return 0;
  }
  return 1;
}

int main() {
  if (check(0))   goto fail;
  goto ok;
fail:
  printf("Failed correctness test\n");
ok:
  return 0;
}
