#include "unity.h"
#include <stdlib.h>
#include <stdint.h>

int32_t mutateArray(uint8_t arr[4]);
int32_t passAlong(uint8_t arr[4]);
int32_t copyArray(uint8_t arr[4]);
int32_t dynamicArray(uint8_t arr[], uint32_t len);

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
