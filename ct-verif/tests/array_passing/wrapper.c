#include <smack.h>
#include "ct-verif.h"
#include <stdint.h>

int32_t mutateIfNot5_wrapper(uint8_t a1, uint8_t a2, uint8_t a3, uint8_t a4, int32_t cond) {
  uint8_t arr[4] = {a1, a2, a3, a4};
  mutateIf5(arr, cond); 
}
