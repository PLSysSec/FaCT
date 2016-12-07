#include "Unity/src/unity.h"
#include <stdlib.h>

int get100();
int getByteArrIndex();
int setByteArrIndex();
int mutateArray(int*);
int mutateArray2(int*,int);
int identity(int);
int simpleIf(int);
int mediumComplexIf(int);
int mixedIf(int);
int mixedIf2(int);
int nestedIf(int);
int simpleLoop();
int loopAcc();
int loopAssignArray(int*);
int add(int, int);
int add10And20();
int addAll(int*);
int callAddAll();
int multiply(int,int);
int equal(int,int);
int nequal(int,int);
int lshift(int,int);
int rshift(int,int);
int gt(int,int);
int gte(int,int);
int lt(int,int);
int lte(int,int);

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
  int i = 444;
  TEST_ASSERT_EQUAL(444,mutateArray2(myarr2,i));
}

void test_identity(void) {
  int i = 1;
  TEST_ASSERT_EQUAL(1,identity(i));
}

void test_simple_if(void) {
  int one = 1;
  int eleven = 11;
  TEST_ASSERT_EQUAL(1,simpleIf(one));
  TEST_ASSERT_EQUAL(2,simpleIf(eleven));
}

void test_medium_complex_if(void) {
  int one = 1;
  int eleven = 11;
  TEST_ASSERT_EQUAL(1,mediumComplexIf(one));
  TEST_ASSERT_EQUAL(2,mediumComplexIf(eleven));
}

void test_mixed_if(void) {
  int ten = 10;
  int eleven = 11;
  TEST_ASSERT_EQUAL(1,mixedIf(ten));
  TEST_ASSERT_EQUAL(2,mixedIf(eleven));
}

void test_mixed_if2(void) {
  int one = 1;
  int eleven = 11;
  TEST_ASSERT_EQUAL(1,mixedIf2(one));
  TEST_ASSERT_EQUAL(2,mixedIf2(eleven));
}

void test_nested_if(void) {
  int one = 1;
  int seven = 7;
  int eleven = 11;
  int sixteen = 16;
  TEST_ASSERT_EQUAL(1,nestedIf(one));
  TEST_ASSERT_EQUAL(2,nestedIf(seven));
  TEST_ASSERT_EQUAL(3,nestedIf(eleven));
  TEST_ASSERT_EQUAL(4,nestedIf(sixteen));
}

void test_simple_loop(void) {
  TEST_ASSERT_EQUAL(10000, simpleLoop());
}

void test_loop_acc(void) {
  TEST_ASSERT_EQUAL(5, loopAcc());
}

void test_loop_assign(void) {
  int arr[5] = {0,0,0,0,0};
  loopAssignArray(arr);
  for(int i = 0; i < 5; i++) {
    TEST_ASSERT_EQUAL(i,arr[i]);
  }
}

void test_add(void) {
  int one = 1;
  int two = 2;
  TEST_ASSERT_EQUAL(3,add(one,two));
}

void test_add_ten_and_twenty(void) {
  TEST_ASSERT_EQUAL(30,add10And20());
}

void test_add_all(void) {
  int arr[5] = {1,2,3,4,5};
  TEST_ASSERT_EQUAL(15,addAll(arr));
}

void test_call_add_all(void) {
  TEST_ASSERT_EQUAL(5,callAddAll());
}

void test_multiply(void) {
  TEST_ASSERT_EQUAL(1,multiply(1,1));
  TEST_ASSERT_EQUAL(5,multiply(1,5));
  TEST_ASSERT_EQUAL(11111155,multiply(1,11111155));
  TEST_ASSERT_EQUAL(18,multiply(2,9));
  TEST_ASSERT_EQUAL(0,multiply(1,0));
}

void test_equal(void) {
  TEST_ASSERT_EQUAL(-1,equal(1,1));
  TEST_ASSERT_EQUAL(0,equal(1,11));
}

void test_nequal(void) {
  TEST_ASSERT_EQUAL(0,nequal(1,1));
  TEST_ASSERT_EQUAL(-1,nequal(1,11));
}

void test_lshift(void) {
  TEST_ASSERT_EQUAL(0,lshift(0,1));
  TEST_ASSERT_EQUAL(2,lshift(1,1));
  TEST_ASSERT_EQUAL(8,lshift(1,3));
  TEST_ASSERT_EQUAL(64,lshift(4,4));
}

void test_rshift(void) {
  TEST_ASSERT_EQUAL(0,rshift(0,1));
  TEST_ASSERT_EQUAL(0,rshift(1,1));
  TEST_ASSERT_EQUAL(4,rshift(8,1));
  TEST_ASSERT_EQUAL(4,rshift(64,4));
}

void test_gt(void) {
  TEST_ASSERT_EQUAL(0,gt(1,1));
  TEST_ASSERT_EQUAL(0,gt(0,1));
  TEST_ASSERT_EQUAL(-1,gt(1,0));
  TEST_ASSERT_EQUAL(-1,gt(111,11));
}

void test_gte(void) {
  TEST_ASSERT_EQUAL(-1,gte(1,1));
  TEST_ASSERT_EQUAL(0,gte(0,1));
  TEST_ASSERT_EQUAL(-1,gte(1,0));
  TEST_ASSERT_EQUAL(-1,gte(111,11));
}

void test_lt(void) {
  TEST_ASSERT_EQUAL(0,lt(1,1));
  TEST_ASSERT_EQUAL(-1,lt(0,1));
  TEST_ASSERT_EQUAL(0,lt(1,0));
  TEST_ASSERT_EQUAL(0,lt(111,11));
}

void test_lte(void) {
  TEST_ASSERT_EQUAL(-1,lte(1,1));
  TEST_ASSERT_EQUAL(-1,lte(0,1));
  TEST_ASSERT_EQUAL(0,lte(1,0));
  TEST_ASSERT_EQUAL(0,lte(111,11));
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
  RUN_TEST(test_simple_loop);
  RUN_TEST(test_loop_acc);
  RUN_TEST(test_loop_assign);
  RUN_TEST(test_add);
  RUN_TEST(test_add_ten_and_twenty);
  RUN_TEST(test_add_all);
  RUN_TEST(test_call_add_all);
  RUN_TEST(test_multiply);
  RUN_TEST(test_equal);
  RUN_TEST(test_nequal);
  RUN_TEST(test_lshift);
  RUN_TEST(test_rshift);
  RUN_TEST(test_gt);
  RUN_TEST(test_gte);
  RUN_TEST(test_lt);
  RUN_TEST(test_lte);
  return UNITY_END();
}
