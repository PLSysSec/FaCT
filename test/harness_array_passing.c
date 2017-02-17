#include "unity.h"
#include <stdlib.h>
#include <stdint.h>

int32_t mutateArray(uint8_t arr[4]);
int32_t passAlong(uint8_t arr[4]);
int32_t copyArray(uint8_t arr[4]);
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
