#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdint.h>

uint32_t sum_array(uint8_t *arr, uint32_t arr_len);

int check(int num) {
  uint8_t *arr = malloc(num);
  uint32_t sum = 0;
  for (uint32_t i = 0; i < num; i++) {
    uint8_t b = (uint8_t)rand();
    arr[i] = b;
    sum += b;
  }
  if(sum != sum_array(arr, num))
    return 1;
  return 0;
}

int main() {
  srand(time(NULL));
  for (uint32_t i = 0; i < 20; i++) {
    uint32_t sz = rand() % 4096;
    if (check(sz)) goto fail;
  }
  goto ok;
fail:
  printf("Failed correctness test\n");
ok:
  return 0;
}
