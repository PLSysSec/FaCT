#include "Unity/src/unity.h"

int get100();
int getByteArrIndex();
int setByteArrIndex();
int mutateArray();
//int mutateArray2();

void test_get(void) {
  TEST_ASSERT_EQUAL(100, get100());
}

void test_get_index(void) {
  TEST_ASSERT_EQUAL(111,getByteArrIndex());
}

void test_set_index(void) {
  TEST_ASSERT_EQUAL(44,setByteArrIndex());
}

void test_mutate_array(void) {
  int myarr[5] = {1,2,3,4,5};
  TEST_ASSERT_EQUAL(44,mutateArray(myarr));
}

void test_mutate_array2(void) {
  int myarr[5] = {1,2,3,4,5};
  //TEST_ASSERT_EQUAL(4,mutateArray2(*myarr,4));
}

int main(void) {
  UNITY_BEGIN();
  RUN_TEST(test_get);
  RUN_TEST(test_get_index);
  RUN_TEST(test_set_index);
  RUN_TEST(test_mutate_array);
  //RUN_TEST(test_mutate_array2);
  return UNITY_END();
}
