#include <stdint.h>

uint32_t mutateArray(uint8_t arr[4]) {
  for(int i = 0; i < 4; i++) {
    arr[i] = 22;
  }
  return 0;
}

uint32_t passAlong(uint8_t arr[4]) {
  return mutateArray(arr);
}

uint32_t copyArray(uint8_t arr[4]) {
  uint8_t myarr[4] = {0,0,0,0};
  int32_t dummy = mutateArray(myarr);
  for(int i = 0; i < 4; i++) {
    arr[i] = myarr[i];
  }
  return 0;
}

int32_t dynamicArray(uint8_t *arr, uint32_t len) {
  for(int i = 0; i < len; i++) {
    arr[i] = arr[i] + 1;
  }
  return 0;
}

int32_t mutateIf5(uint8_t arr[4], int32_t cond) {
  int32_t dummy = 0;
  if(cond == 5) {
    dummy = mutateArray(arr);
  } else {
    dummy = 0;
  }
  return 0;
}

int32_t mutateIfNot5(uint8_t arr[4], int32_t cond) {
  int32_t dummy = 0;
  if(cond == 5) {
    dummy = 0;
  } else {
    dummy = mutateArray(arr);
  }
  return 0;
}
