#include "Unity/src/unity.h"

int get100();
int getByteArrIndex();
int setByteArrIndex();

void test_get(void) {
  TEST_ASSERT_EQUAL(100, get100());
}

void test_get_index(void) {
  TEST_ASSERT_EQUAL(111,getByteArrIndex());
}

void test_set_index(void) {
  TEST_ASSERT_EQUAL(44,setByteArrIndex());
}

int main(void) {
  UNITY_BEGIN();
  RUN_TEST(test_get);
  RUN_TEST(test_get_index);
  RUN_TEST(test_set_index);
  return UNITY_END();
}

/*

  bytearr[2] myarry = {0x01, 0xff}


*/
