#include "unity.h"
#include <stdlib.h>
#include <stdint.h>

struct a_sample_thing {
  /*secret*/ int32_t a_field;
  /*public*/ int32_t unused;
  /*secret*/ int32_t b_field;
};

struct a_sample_thing;

#include "test_structs.h"

void test_duplicateCall(void) {
  struct a_sample_thing ss = {0};
  TEST_ASSERT_EQUAL(1, dummy(2, &ss));
}
