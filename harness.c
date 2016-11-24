#include "Unity/src/unity.h"
#include <stdlib.h>

int get100();
int getByteArrIndex();
int setByteArrIndex();
int mutateArray();
int mutateArray2();
int identity();
int simpleIf();

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
  int myarr2[5] = {1,2,3,4,5};
  int *i = malloc(sizeof(int));
  *i = 444;
  TEST_ASSERT_EQUAL(444,mutateArray2(myarr2,i));
}

void test_identity(void) {
  int *i = malloc(sizeof(int));
  *i = 1;
  TEST_ASSERT_EQUAL(1,identity(i));
}

void test_simple_if(void) {
  int *one = malloc(sizeof(int));
  *one = 1;
  int *eleven = malloc(sizeof(int));
  *eleven = 11;
  TEST_ASSERT_EQUAL(1,simpleIf(one));
  TEST_ASSERT_EQUAL(2,simpleIf(eleven));
}

int main(void) {
  UNITY_BEGIN();
  RUN_TEST(test_get);
  RUN_TEST(test_get_index);
  RUN_TEST(test_set_index);
  RUN_TEST(test_mutate_array);
  RUN_TEST(test_mutate_array2);
  RUN_TEST(test_identity);
  RUN_TEST(test_simple_if);
  return UNITY_END();
}
