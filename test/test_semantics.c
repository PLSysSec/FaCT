#include <stdint.h>

int32_t get100() {
  int32_t x = 666;
  int32_t x2 = 100;
  x = 10;
  return x2;
}

int32_t identity(int32_t my_int) {
  return my_int;
}

int32_t mutateArray(int32_t arr[5]) {
  arr[4] = 44;
  return arr[4];
}

int32_t mutateArray2(int32_t arr2[5], int32_t val) {
  arr2[4] = val;
  uint32_t f = 4;
  return arr2[f];
}

int32_t simpleIf(int32_t cond) {
  if (10 > cond) {
    return 1;
  } else {
    return 2;
  }
}

int32_t mediumComplexIf(int32_t cond) {
  int32_t complex_ret = 10;
  if (10 > cond) {
    complex_ret = 1;
  } else {
    complex_ret = 2;
  }
  return complex_ret;
}

int32_t mixedIf(int32_t cond) {
  int32_t complex_ret = 10;
  if (10 == cond) {
    return 1;
  } else {
    complex_ret = 2;
  }
  return complex_ret;
}

int32_t mixedIf2(int32_t cond) {
  int32_t complex_ret = 10;
  if (10 > cond) {
    complex_ret = 1;
  } else {
    return 2;
  }
  return complex_ret;
}

int32_t nestedIf(int32_t cond) {
  if (10 > cond) {
    if (5 > cond) {
      return 1;
    } else {
      return 2;
    }
  } else {
    if (15 > cond) {
      return 3;
    } else {
      return 4;
    }
  }
}

int32_t simpleLoop() {
  for (uint32_t i = 0; i < 10; i++) {
    int32_t a = 666;
  }
  return 10000;
}

int32_t loopAcc() {
  int32_t acc = 0;
  for (uint32_t i = 0; i < 5; i++) {
    acc = acc + 1;
  }
  return acc;
}

int32_t loopAssignArray(uint32_t arr[5]) {
  for (uint32_t i = 0; i < 5; i++) {
    arr[i] = i;
  }
  return 10000;
}

int32_t add(int32_t a, int32_t b) {
  return a + b;
}

int32_t add10And20() {
  return add(10, 20);
}

int32_t addAll(int32_t arr[5]) {
  int32_t acc = 0;
  for (uint32_t i = 0; i < 5; i++) {
    acc = acc + arr[i];
  }
  return acc;
}

int32_t multiply(int32_t a, int32_t b) {
  return a * b;
}

int8_t equal(int32_t a, int32_t b) {
  return a == b;
}

int8_t nequal(int32_t a, int32_t b) {
  return a != b;
}

int32_t lshift(int32_t num, uint32_t shift) {
  return num << shift;
}

int32_t rshift(int32_t num, uint32_t shift) {
  return num >> shift;
}

int8_t gt(int32_t a, int32_t b) {
  return a > b;
}

int8_t gte(int32_t a, int32_t b) {
  return a >= b;
}

int8_t lt(int32_t a, int32_t b) {
  return a < b;
}

int8_t lte(int32_t a, int32_t b) {
  return a <= b;
}

int32_t neg(int32_t a) {
  return -a;
}

int32_t xor(int32_t a, int32_t b) {
  return a ^ b;
}

int32_t prec(int32_t a) {
  if (a == 1) {
    return 2 + 3 * 4;
  } else {
    if (a == 2) {
      return (2 + 3) * 4;
    } else {
      if (a == 3) {
        return -2 + 3;
      } else {
        return -1;
      }
    }
  }
}

int32_t opassign() {
  int32_t a = 1;
  a += 5; // 6
  a -= 2; // 4
  a *= 3; // 12
  a <<= 3; // 12 * 8
  a >>= 2; // 24 == 0b11000
  a &= 0b101011; // 0b1000
  a |= 0b0101; // 0b1101
  a ^= 0b0110; // 0b1011 == 11
  return a;
}

int32_t add5int8(int8_t num) {
  return num + 5;
}

int32_t complicatedAdd5(int32_t num) {
  int8_t five = 5;
  int32_t one = 1;
  int32_t six = five + 1;
  int8_t smallSix = 1 + 5;
  return num + five;
}

uint32_t add5uint32_t(uint32_t num) {
  return num + 5;
}

uint16_t add5uint16(uint16_t num) {
  return num + 5;
}

uint32_t add5uintUnify(uint16_t num) {
  return num + 5;
}
