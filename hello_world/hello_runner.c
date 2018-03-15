#include <stdio.h>
#include <stdint.h>

#include "hello_world.h"

int main() {
  uint8_t cond = 1;
  uint32_t x = 42;
  uint32_t y = 137;

  printf("cond: %u, x: %u, y: %u\n", cond, x, y);

  conditional_swap(&x, &y, cond);

  printf("after swap:\n");
  printf("cond: %u, x: %u, y: %u\n", cond, x, y);

  return 0;
}
