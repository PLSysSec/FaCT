#include "unity.h"
#include <stdlib.h>
#include <stdint.h>
#include "test_structs.h"

void test_duplicateCall(void) {
  TEST_ASSERT_EQUAL(1, dummy());
}
