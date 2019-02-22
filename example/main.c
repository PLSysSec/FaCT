#include <stdio.h>
#include <stdint.h>

#include "example.h"

int main() {
  uint32_t a = 221;
  uint32_t b = 24;

  printf("choose(0, 221, 24) => %u\n", choose(0, a, b));
  printf("choose(1, 221, 24) => %u\n", choose(1, a, b));

  return 0;
}
