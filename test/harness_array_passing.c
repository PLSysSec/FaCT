#include "unity.h"
#include <stdlib.h>
#include <stdint.h>

int32_t passAlong(uint8_t arr[4]);

void test_passalong() {
  uint8_t arr[4] = {2, 3, 4, 5};
  passAlong(arr);
  TEST_ASSERT_EQUAL(22, arr[0]);
  TEST_ASSERT_EQUAL(22, arr[1]);
  TEST_ASSERT_EQUAL(22, arr[2]);
  TEST_ASSERT_EQUAL(22, arr[3]);
}
