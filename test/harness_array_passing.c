#include "unity.h"
#include <stdlib.h>
#include <stdint.h>
#include "test_array_passing.h"

void test_mutatearray() {
  uint8_t arr[4] = {2, 3, 4, 5};
  mutateArray(arr,1);
  TEST_ASSERT_EQUAL(22, arr[0]);
  TEST_ASSERT_EQUAL(22, arr[1]);
  TEST_ASSERT_EQUAL(22, arr[2]);
  TEST_ASSERT_EQUAL(22, arr[3]);
}

void test_passalong() {
  uint8_t arr[4] = {2, 3, 4, 5};
  passAlong(arr,1);
  TEST_ASSERT_EQUAL(22, arr[0]);
  TEST_ASSERT_EQUAL(22, arr[1]);
  TEST_ASSERT_EQUAL(22, arr[2]);
  TEST_ASSERT_EQUAL(22, arr[3]);
}

void test_copyarray() {
  uint8_t arr[4] = {2, 3, 4, 5};
  copyArray(arr,1);
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

  dynamicArray(arr, 7, 1);

  for (uint32_t i = 0; i < 7; i++) {
    TEST_ASSERT_EQUAL(i + 1, arr[i]);
  }
}

void test_mutate_if() {
  uint8_t arr[4] = {2, 3, 4, 5};
  mutateIf5(arr, 6, 1);
  for (int i = 0; i < 4; i++)
    TEST_ASSERT_EQUAL(i + 2, arr[i]);
  mutateIf5(arr, 5, 1);
  for (int i = 0; i < 4; i++)
    TEST_ASSERT_EQUAL(22, arr[i]);
}

void test_mutate_if_swapped() {
  uint8_t arr[4] = {2, 3, 4, 5};
  mutateIfNot5(arr, 6, 1);
  for (int i = 0; i < 4; i++) {
    TEST_ASSERT_EQUAL(22, arr[i]);
    arr[i] = i + 2;
  }
  mutateIfNot5(arr, 5, 1);
  for (int i = 0; i < 4; i++)
    TEST_ASSERT_EQUAL(i + 2, arr[i]);
}
