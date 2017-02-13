#include "unity.h"
#include <stdlib.h>
#include <stdint.h>

int32_t mutateRef(uint8_t* p);
int32_t passAlong(uint8_t* p);

void test_mutateref() {
  uint8_t c = 0;
  mutateRef(&c);
  TEST_ASSERT_EQUAL(22, c);
}

void test_passalong() {
  uint8_t c = 0;
  passAlong(&c);
  TEST_ASSERT_EQUAL(22, c);
}
