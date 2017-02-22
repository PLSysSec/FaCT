#include <smack.h>
#include "ct-verif.h"
#include <stdint.h>

//#include "test_array_passing.c"


int32_t mutateIfNot5(uint8_t arr[4], int32_t cond);

int32_t mutateIfNot5_wrapper(uint8_t a1, uint8_t a2, uint8_t a3, uint8_t a4, int32_t cond) {
  uint8_t arr[4] = {a1, a2, a3, a4};
  return mutateIfNot5(arr, cond);
}

/*
// File: harness_array_passing.c

int32_t mutateArray(uint8_t arr[4]);
int32_t passAlong(uint8_t arr[4]);
int32_t copyArray(uint8_t arr[4]);
int32_t dynamicArray(uint8_t arr[], uint32_t len);
int32_t mutateIf5(uint8_t arr[4], int32_t cond);
int32_t mutateIfNot5(uint8_t arr[4], int32_t cond);

void test_mutatearray() {
  uint8_t arr[4] = {2, 3, 4, 5};
  mutateArray(arr);
  TEST_ASSERT_EQUAL(22, arr[0]);
  TEST_ASSERT_EQUAL(22, arr[1]);
  TEST_ASSERT_EQUAL(22, arr[2]);
  TEST_ASSERT_EQUAL(22, arr[3]);
}

void test_passalong() {
  uint8_t arr[4] = {2, 3, 4, 5};
  passAlong(arr);
  TEST_ASSERT_EQUAL(22, arr[0]);
  TEST_ASSERT_EQUAL(22, arr[1]);
  TEST_ASSERT_EQUAL(22, arr[2]);
  TEST_ASSERT_EQUAL(22, arr[3]);
}

void test_copyarray() {
  uint8_t arr[4] = {2, 3, 4, 5};
  copyArray(arr);
  TEST_ASSERT_EQUAL(22, arr[0]);
  TEST_ASSERT_EQUAL(22, arr[1]);
  TEST_ASSERT_EQUAL(22, arr[2]);
  TEST_ASSERT_EQUAL(22, arr[3]);
}

void test_dynamicarray() {
  uint8_t arr[7];

  for (uint32_t i = 0; i < 7; i++) {
    arr[i] = i;
  }

  dynamicArray(arr, 7);

  for (uint32_t i = 0; i < 7; i++) {
    TEST_ASSERT_EQUAL(i + 1, arr[i]);
  }
}

void test_mutate_if() {
  uint8_t arr[4] = {2, 3, 4, 5};
  mutateIf5(arr, 6);
  for (int i = 0; i < 4; i++)
    TEST_ASSERT_EQUAL(i + 2, arr[i]);
  mutateIf5(arr, 5);
  for (int i = 0; i < 4; i++)
    TEST_ASSERT_EQUAL(22, arr[i]);
}

void test_mutate_if_swapped() {
  uint8_t arr[4] = {2, 3, 4, 5};
  mutateIfNot5(arr, 6);
  for (int i = 0; i < 4; i++) {
    TEST_ASSERT_EQUAL(22, arr[i]);
    arr[i] = i + 2;
  }
  mutateIfNot5(arr, 5);
  for (int i = 0; i < 4; i++)
    TEST_ASSERT_EQUAL(i + 2, arr[i]);
}
*/
