#include "Unity/src/unity.h"
#include <stdlib.h>

int get100();
int getByteArrIndex();
int setByteArrIndex();
int mutateArray(int*);
int mutateArray2(int*,int*);
int identity(int*);
int simpleIf(int*);
int mediumComplexIf(int*);
int mixedIf(int*);
int mixedIf2(int*);
int nestedIf(int*);

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

void test_medium_complex_if(void) {
  int *one = malloc(sizeof(int));
  *one = 1;
  int *eleven = malloc(sizeof(int));
  *eleven = 11;
  TEST_ASSERT_EQUAL(1,mediumComplexIf(one));
  TEST_ASSERT_EQUAL(2,mediumComplexIf(eleven));
}

void test_mixed_if(void) {
  int *one = malloc(sizeof(int));
  *one = 1;
  int *eleven = malloc(sizeof(int));
  *eleven = 11;
  TEST_ASSERT_EQUAL(1,mixedIf(one));
  TEST_ASSERT_EQUAL(2,mixedIf(eleven));
}

void test_mixed_if2(void) {
  int *one = malloc(sizeof(int));
  *one = 1;
  int *eleven = malloc(sizeof(int));
  *eleven = 11;
  TEST_ASSERT_EQUAL(1,mixedIf2(one));
  TEST_ASSERT_EQUAL(2,mixedIf2(eleven));
}

void test_nested_if(void) {
  int *one = malloc(sizeof(int));
  *one = 1;
  int *seven = malloc(sizeof(int));
  *seven = 7;
  int *eleven = malloc(sizeof(int));
  *eleven = 11;
  int *sixteen = malloc(sizeof(int));
  *sixteen = 16;
  TEST_ASSERT_EQUAL(1,nestedIf(one));
  TEST_ASSERT_EQUAL(2,nestedIf(seven));
  TEST_ASSERT_EQUAL(3,nestedIf(eleven));
  TEST_ASSERT_EQUAL(4,nestedIf(sixteen));
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
  RUN_TEST(test_medium_complex_if);
  RUN_TEST(test_mixed_if);
  RUN_TEST(test_mixed_if2);
  RUN_TEST(test_nested_if);
  return UNITY_END();
}
