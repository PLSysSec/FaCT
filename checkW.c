#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int hello_world();

int check() {
  return hello_world();
}

int main() {
  if (check())   goto fail;
  goto ok;
fail:
  printf("Failed correctness test\n");
ok:
  return 0;
}
