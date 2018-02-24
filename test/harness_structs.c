#include "unity.h"
#include <stdlib.h>
#include <stdint.h>

struct a_sample_thing {
  /*secret*/ int32_t a_field;
  /*public*/ int32_t unused;
  /*secret*/ int32_t b_field;
};

#include "test_structs.h"

void test_passing_struct(void) {
  struct a_sample_thing ss = {0};
  TEST_ASSERT_EQUAL(1, recv_struct(2, &ss));
}

void test_access_struct(void) {
  struct a_sample_thing ss = { 3, 8, 9 };
  TEST_ASSERT_EQUAL(9, access_struct(0, &ss));
  TEST_ASSERT_EQUAL(3, access_struct(1, &ss));
}

void test_blitz_struct(void) {
  struct a_sample_thing ss = { 3, 8, 9 };
  TEST_ASSERT_EQUAL(9, blitz_struct(0, &ss));
  TEST_ASSERT_EQUAL(3, blitz_struct(1, &ss));
}

void test_assign_struct(void) {
  struct a_sample_thing ss = {0};
  set_struct(0, 6, &ss);
  TEST_ASSERT_EQUAL(0, ss.a_field);
  TEST_ASSERT_EQUAL(0, ss.unused);
  TEST_ASSERT_EQUAL(6, ss.b_field);
  set_struct(1, 4, &ss);
  TEST_ASSERT_EQUAL(4, ss.a_field);
  TEST_ASSERT_EQUAL(0, ss.unused);
  TEST_ASSERT_EQUAL(6, ss.b_field);
}

void test_assign_blitz(void) {
  struct a_sample_thing ss = {0};
  set_struct(1, 4, &ss);
  TEST_ASSERT_EQUAL(4, ss.a_field);
  TEST_ASSERT_EQUAL(0, ss.unused);
  TEST_ASSERT_EQUAL(0, ss.b_field);
  set_struct(0, 6, &ss);
  TEST_ASSERT_EQUAL(4, ss.a_field);
  TEST_ASSERT_EQUAL(0, ss.unused);
  TEST_ASSERT_EQUAL(6, ss.b_field);
}
