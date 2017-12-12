#include "unity.h"
#include <stdlib.h>
#include <stdint.h>
#include "test_regression.h"

void test_duplicateCall(void) {
  TEST_ASSERT_EQUAL(0, duplicateCall());
}