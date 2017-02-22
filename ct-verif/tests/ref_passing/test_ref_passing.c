#include <stdint.h>

int32_t mutateRef(uint8_t c) {
  c = 22;
  return 0;
}

int32_t passAlong(uint8_t c) {
  return mutateRef(c);
}

int32_t mutateIf5(int32_t cond, uint8_t c) {
  int32_t dummy = 0;
  if (cond == 5) {
    dummy = mutateRef(c);
  } else {
    dummy = 0;
  }
  return 0;
}

int32_t mutateIfNot5(int32_t cond, uint8_t c) {
  int32_t dummy = 0;
  if (cond == 5) {
    dummy = 0;
  } else {
    dummy = mutateRef(c);
  }
  return 0;
}

int32_t mutateIf5ElseReturn(int32_t cond, uint8_t c) {
  int32_t dummy = 0;
  if (cond != 5) {
    return 1;
  } else {
    return mutateRef(c);
  }
}
