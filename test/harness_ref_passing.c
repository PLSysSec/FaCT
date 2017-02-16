#include "unity.h"
#include <stdlib.h>
#include <stdint.h>

int32_t mutateRef(uint8_t* p);
int32_t passAlong(uint8_t* p);
int32_t mutateIf5(int32_t cond, uint8_t* p);
int32_t mutateIfNot5(int32_t cond, uint8_t* p);

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

void test_mutate_if() {
  uint8_t c = 0;
  mutateIf5(4, &c);
  TEST_ASSERT_EQUAL(0, c);
  mutateIf5(5, &c);
  TEST_ASSERT_EQUAL(22, c);
}

void test_mutate_if_swapped() {
  uint8_t c = 0;
  mutateIfNot5(4, &c);
  TEST_ASSERT_EQUAL(22, c);
  c = 0;
  mutateIfNot5(5, &c);
  TEST_ASSERT_EQUAL(0, c);
}
