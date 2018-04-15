#include "unity.h"
#include <stdlib.h>
#include <stdint.h>
#include "test_semantics.h"

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
  uint32_t arr[5] = {0,0,0,0,0};
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
  TEST_ASSERT_EQUAL(255,equal(1,1));
  TEST_ASSERT_EQUAL(0,equal(1,11));
}

void test_nequal(void) {
  TEST_ASSERT_EQUAL(0,nequal(1,1));
  TEST_ASSERT_EQUAL(255,nequal(1,11));
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
  TEST_ASSERT_EQUAL(255,gt(1,0));
  TEST_ASSERT_EQUAL(255,gt(111,11));
}

void test_gte(void) {
  TEST_ASSERT_EQUAL(255,gte(1,1));
  TEST_ASSERT_EQUAL(0,gte(0,1));
  TEST_ASSERT_EQUAL(255,gte(1,0));
  TEST_ASSERT_EQUAL(255,gte(111,11));
}

void test_lt(void) {
  TEST_ASSERT_EQUAL(0,lt(1,1));
  TEST_ASSERT_EQUAL(255,lt(0,1));
  TEST_ASSERT_EQUAL(0,lt(1,0));
  TEST_ASSERT_EQUAL(0,lt(111,11));
}

void test_lte(void) {
  TEST_ASSERT_EQUAL(255,lte(1,1));
  TEST_ASSERT_EQUAL(255,lte(0,1));
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

void test_summZero(void) {
  TEST_ASSERT_EQUAL(0, summZero());
}

void test_summNonZero(void) {
  TEST_ASSERT_EQUAL(6, summNonZero());
}

void test_summZeroDynamic(void) {
  TEST_ASSERT_EQUAL(0, summZeroDynamic());
}

void test_summNonZeroDynamic(void) {
  TEST_ASSERT_EQUAL(6, summNonZeroDynamic());
}

void test_summCopyZero(void) {
  TEST_ASSERT_EQUAL(0, summCopyZero());
}

void test_summCopyNonZero(void) {
  TEST_ASSERT_EQUAL(15, summCopyNonZero());
}

void test_summCopyZeroDynamic(void) {
  TEST_ASSERT_EQUAL(0, summCopyZeroDynamic());
}

void test_summCopyNonZeroDynamic(void) {
  TEST_ASSERT_EQUAL(15, summCopyNonZeroDynamic());
}

void test_summViewZero(void) {
  TEST_ASSERT_EQUAL(0, summViewZero());
}

void test_summViewNonZero(void) {
  TEST_ASSERT_EQUAL(3, summViewNonZero());
}

void test_summViewDynamic(void) {
  int arr1[5] = {1,2,3,4,5};
  TEST_ASSERT_EQUAL(6, summViewDynamic(arr1,5));
  int arr2[5] = {5,4,3,2,1};
  TEST_ASSERT_EQUAL(12, summViewDynamic(arr2,5));
}

void test_arrget1(void) {
  TEST_ASSERT_EQUAL(1, arrget1());
}

void test_arrget2(void) {
  TEST_ASSERT_EQUAL(2, arrget2());
}

void test_arrget3(void) {
  TEST_ASSERT_EQUAL(3, arrget3());
}

void test_arrget4(void) {
  TEST_ASSERT_EQUAL(10, arrget4());
}

void test_arrgetDynamic1(void) {
  int arr[] = {1,2,3,4,5,6,7};
  TEST_ASSERT_EQUAL(1, arrgetDynamic1(arr,7));
}

void test_arrgetDynamic2(void) {
  int arr[] = {1,2,3,4,5,6,7};
  TEST_ASSERT_EQUAL(2, arrgetDynamic2(arr,7));
}

void test_arrgetDynamic3(void) {
  int arr[] = {1,2,3,4,5,6,7};
  TEST_ASSERT_EQUAL(3, arrgetDynamic3(arr,7));
}

void test_arrgetDynamic4(void) {
  int arr[] = {1,2,3,4,5,6,7};
  TEST_ASSERT_EQUAL(7, arrgetDynamic4(arr,7));
}

void test_mutateRef(void) {
  int a = 0;
  mutateRef(&a);
  TEST_ASSERT_EQUAL(5, a);
}

void test_mutateRefCall(void) {
  TEST_ASSERT_EQUAL(5, mutateRefCall());
}

void test_simpleArrAccess(void) {
  TEST_ASSERT_EQUAL(1, simpleArrAccess());
}

void test_simpleArrZerosAccess(void) {
  TEST_ASSERT_EQUAL(0, simpleArrZerosAccess());
}

void test_simpleArrViewAccess(void) {
  TEST_ASSERT_EQUAL(3, simpleArrViewAccess());
}

void test_paramArrAccess(void) {
  int arr[5] = {1,2,3,4,5};
  TEST_ASSERT_EQUAL(5, paramArrAccess(arr));
}

void test_paramArrAccessDyn(void) {
  int arr[5] = {1,2,3,4,5};
  TEST_ASSERT_EQUAL(5, paramArrAccessDyn(arr,5));
}

void test_paramArrViewAccess(void) {
  int arr[5] = {1,2,3,4,5};
  TEST_ASSERT_EQUAL(4, paramArrViewAccess(arr));
}

void test_paramArrViewAccessDyn(void) {
  int arr[5] = {1,2,3,4,5};
  TEST_ASSERT_EQUAL(2, paramArrViewAccessDyn(arr,5));
}

void test_simpleArrayMutation(void) {
  int arr[5] = {1,2,3,4,5};
  simpleArrayMutation(arr,5);
  TEST_ASSERT_EQUAL(2, arr[0]);
  TEST_ASSERT_EQUAL(4, arr[1]);
  TEST_ASSERT_EQUAL(6, arr[2]);
  TEST_ASSERT_EQUAL(8, arr[3]);
  TEST_ASSERT_EQUAL(10, arr[4]);
}

void test_complexArrayMutation(void) {
  int arr[5] = {1,2,3,4,5};
  complexArrayMutation(arr);
  TEST_ASSERT_EQUAL(2, arr[0]);
  TEST_ASSERT_EQUAL(4, arr[1]);
  TEST_ASSERT_EQUAL(6, arr[2]);
  TEST_ASSERT_EQUAL(8, arr[3]);
  TEST_ASSERT_EQUAL(10, arr[4]);
}

void test_complexArrayViewMutation(void) {
  int arr[10] = {1,2,3,4,5,6,7,8,9,10};
  complexArrayViewMutation(arr);
  TEST_ASSERT_EQUAL(2, arr[0]);
  TEST_ASSERT_EQUAL(4, arr[1]);
  TEST_ASSERT_EQUAL(6, arr[2]);
  TEST_ASSERT_EQUAL(8, arr[3]);
  TEST_ASSERT_EQUAL(10, arr[4]);
  TEST_ASSERT_EQUAL(6, arr[5]);
  TEST_ASSERT_EQUAL(7, arr[6]);
  TEST_ASSERT_EQUAL(8, arr[7]);
  TEST_ASSERT_EQUAL(9, arr[8]);
  TEST_ASSERT_EQUAL(10, arr[9]);
}

void test_complexArrayViewRead(void) {
  int arr[10] = {1,2,3,4,5,6,7,8,9,10};
  TEST_ASSERT_EQUAL(1, complexArrayViewRead(arr,0));
}

void test_arrcopy(void) {
  int8_t arr[5] = {1,2,3,4,5};
  int8_t arr2[5];
  simpleArrCopy(arr2,5,arr);
  TEST_ASSERT_EQUAL(1, arr2[0]);
  TEST_ASSERT_EQUAL(2, arr2[1]);
  TEST_ASSERT_EQUAL(3, arr2[2]);
  TEST_ASSERT_EQUAL(4, arr2[3]);
  TEST_ASSERT_EQUAL(5, arr2[4]);
}

void test_arrcopy32(void) {
  int arr[5] = {1,2,3,4,5};
  int arr2[5];
  simpleArrCopy32(arr2,5,arr);
  TEST_ASSERT_EQUAL(1, arr2[0]);
  TEST_ASSERT_EQUAL(2, arr2[1]);
  TEST_ASSERT_EQUAL(3, arr2[2]);
  TEST_ASSERT_EQUAL(4, arr2[3]);
  TEST_ASSERT_EQUAL(5, arr2[4]);
}

void test_arrcopyStatic(void) {
  int8_t arr[5] = {1,2,3,4,5};
  int8_t arr2[5];
  simpleArrCopyStatic(arr2,arr);
  TEST_ASSERT_EQUAL(1, arr2[0]);
  TEST_ASSERT_EQUAL(2, arr2[1]);
  TEST_ASSERT_EQUAL(3, arr2[2]);
  TEST_ASSERT_EQUAL(4, arr2[3]);
  TEST_ASSERT_EQUAL(5, arr2[4]);
}

void test_arrcopyStatic32(void) {
  int arr[5] = {1,2,3,4,5};
  int arr2[5];
  simpleArrCopyStatic32(arr2,arr);
  TEST_ASSERT_EQUAL(1, arr2[0]);
  TEST_ASSERT_EQUAL(2, arr2[1]);
  TEST_ASSERT_EQUAL(3, arr2[2]);
  TEST_ASSERT_EQUAL(4, arr2[3]);
  TEST_ASSERT_EQUAL(5, arr2[4]);
}

void test_arrCopy(void) {
  int arr[5] = {11,12,13,14,15};
  TEST_ASSERT_EQUAL(11, arrCopy(arr,0));
  TEST_ASSERT_EQUAL(12, arrCopy(arr,1));
  TEST_ASSERT_EQUAL(13, arrCopy(arr,2));
  TEST_ASSERT_EQUAL(14, arrCopy(arr,3));
  TEST_ASSERT_EQUAL(15, arrCopy(arr,4));
}

void test_mediumComplexArrCopy(void) {
  TEST_ASSERT_EQUAL(1, mediumComplexArrCopy(0));
  TEST_ASSERT_EQUAL(2, mediumComplexArrCopy(1));
  TEST_ASSERT_EQUAL(3, mediumComplexArrCopy(2));
  TEST_ASSERT_EQUAL(4, mediumComplexArrCopy(3));
  TEST_ASSERT_EQUAL(5, mediumComplexArrCopy(4));
}

void test_complexArrCopy(void) {
  TEST_ASSERT_EQUAL(1, complexArrCopy(0));
  TEST_ASSERT_EQUAL(2, complexArrCopy(1));
  TEST_ASSERT_EQUAL(3, complexArrCopy(2));
  TEST_ASSERT_EQUAL(4, complexArrCopy(3));
  TEST_ASSERT_EQUAL(5, complexArrCopy(4));
}
