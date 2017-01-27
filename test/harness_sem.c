#include "unity.h"
#include <stdlib.h>
#include <stdint.h>

int get100();
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
int multiply(int,int);
int equal(int,int);
int nequal(int,int);
int lshift(int,int);
int rshift(int,int);
int gt(int,int);
int gte(int,int);
int lt(int,int);
int lte(int,int);
int neg(int);
int xor(int,int);
int prec(int);
int opassign();
int add5int8(int8_t);
int complicatedAdd5(int);
uint32_t add5uint32(uint32_t);
uint16_t add5uint16(uint16_t);
uint32_t add5uintUnify(uint16_t);

void test_get(void) {
  TEST_ASSERT_EQUAL(100, get100());
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

void test_neg(void) {
  TEST_ASSERT_EQUAL(-1,neg(1));
  TEST_ASSERT_EQUAL(4,neg(-4));
  TEST_ASSERT_NOT_EQUAL(-8,neg(3));
  TEST_ASSERT_EQUAL(0,neg(0));
}

void test_xor(void) {
  TEST_ASSERT_EQUAL(1,xor(2,3));
  TEST_ASSERT_EQUAL(2,xor(1,3));
  TEST_ASSERT_EQUAL(3,xor(2,1));
  TEST_ASSERT_EQUAL(49,xor(15,62));
}

void test_precedence(void) {
  TEST_ASSERT_EQUAL(14,prec(1));
  TEST_ASSERT_EQUAL(20,prec(2));
  TEST_ASSERT_EQUAL(1,prec(3));
}

void test_opassign(void) {
  TEST_ASSERT_EQUAL(11,opassign());
}

void test_add5int8(void) {
  int8_t one = 1;
  int8_t negTen = -10;
  TEST_ASSERT_EQUAL(6,add5int8(one));
  TEST_ASSERT_EQUAL(-5,add5int8(negTen));
}

void test_complicatedAdd5(void) {
  int8_t one = 1;
  int8_t negTen = -10;
  TEST_ASSERT_EQUAL(6,complicatedAdd5(one));
  TEST_ASSERT_EQUAL(-5,complicatedAdd5(negTen));
}

void test_add5uint32(void) {
  uint32_t ten = 10;
  TEST_ASSERT_EQUAL(15,add5uint32(ten));
}

void test_add5uint16(void) {
  uint16_t ten = 10;
  TEST_ASSERT_EQUAL(15,add5uint16(ten));
}

void test_add5uintUnify(void) {
  uint16_t ten = 10;
  uint32_t fifteen = 15;
  TEST_ASSERT_EQUAL(fifteen, add5uintUnify(ten));
}