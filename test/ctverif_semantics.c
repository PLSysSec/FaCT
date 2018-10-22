#include <stdlib.h>
#include <stdint.h>
#include "test_semantics.h"
#include "ctverif.h"

int32_t get100_wrapper(void) {
    return get100();
}

int32_t identity_wrapper(int32_t my_int) {
    return identity(my_int);
}

int32_t mutateArray_wrapper(int32_t arr[5]) {
    public_in(__SMACK_value(arr));
    return mutateArray(arr);
}

int32_t mutateArray2_wrapper(int32_t arr2[5], int32_t val){
    public_in(__SMACK_value(arr2));
    return mutateArray2(arr2, val);
}

int32_t simpleIf_wrapper(int32_t cond) {
    return simpleIf(cond);
}

int32_t mediumComplexIf_wrapper(int32_t cond) {
    return mediumComplexIf(cond);
}

int32_t mixedIf_wrapper(int32_t cond) {
    return mixedIf(cond);
}

int32_t mixedIf2_wrapper(int32_t cond) {
    return mixedIf2(cond);
}

int32_t nestedIf_wrapper(int32_t cond) {
    return nestedIf(cond);
}

/*public return*/
int32_t simpleLoop_wrapper() {
    public_out(__SMACK_return_value());
    return simpleLoop();
}

int32_t loopAcc_wrapper() {
    return loopAcc();
}

/*public return*/
int32_t loopAssignArray_wrapper(uint32_t arr[5]) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(arr));
    return loopAssignArray(arr);
}

/*public return*/
int32_t add_wrapper(/*public*/ int32_t a, /*public*/ int32_t b) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(a));
    public_in(__SMACK_value(b));
    return add(a,b);
}

/*public return*/
int32_t add10And20_wrapper() {
    public_out(__SMACK_return_value());
    return add10And20();
}

int32_t addAll_wrapper(const int32_t arr[5]) {
    public_in(__SMACK_value(arr));
    return addAll(arr);
}

int32_t multiply_wrapper(int32_t a, int32_t b) {
    return multiply(a, b);
}

uint8_t equal_wrapper(int32_t a,int32_t b) {
    return equal(a,b); 
}

uint8_t nequal_wrapper(int32_t a,int32_t b) {
    return nequal(a,b);
}

int32_t lshift_wrapper(int32_t num,uint32_t shift) {
    return lshift(num,shift);
}

int32_t rshift_wrapper(int32_t num,uint32_t shift) {
    return rshift(num,shift);
}

uint8_t gt_wrapper(int32_t a,int32_t b) {
    return gt(a,b);
}

uint8_t gte_wrapper(int32_t a,int32_t b) {
    return gte(a,b);
}

uint8_t lt_wrapper(int32_t a,int32_t b) {
    return lt(a,b);
}

uint8_t lte_wrapper(int32_t a,int32_t b) {
    return lte(a,b);
}

int32_t neg_wrapper(int32_t a) {
    return neg(a);
}

int32_t xor_wrapper(int32_t a,int32_t b) {
     return xor(a,b);
}

int32_t prec_wrapper(int32_t a) {
    return prec(a);
}

int32_t opassign_wrapper() {
    return opassign();
}

/*public return*/
int32_t add5int8_wrapper(/*public*/ int8_t num) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(num));
    return add5int8(num);
}

/*public return*/
int32_t complicatedAdd5_wrapper(/*public*/ int32_t num){ 
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(num));
    return complicatedAdd5(num);
}

/*public return*/
uint32_t add5uint32_wrapper(/*public*/ uint32_t num) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(num));
    return add5uint32(num);
}

/*public return*/
uint16_t add5uint16_wrapper(/*public*/ uint16_t num) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(num));
    return add5uint16(/*public*/ num);
}

/*public return*/
uint32_t add5uintUnify_wrapper(/*public*/ uint16_t num) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(num));
    return add5uintUnify(/*public*/ num);
}

int32_t summ_wrapper(const int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return summ(arr,/*public*/ __arr_len);
}

int32_t summZero_wrapper() {
    return summZero();
}

int32_t summNonZero_wrapper() {
    return summNonZero();
}

int32_t summZeroDynamic_wrapper() {
    return summZeroDynamic();
}

int32_t summNonZeroDynamic_wrapper() {
    return summNonZeroDynamic();
}

int32_t summCopyZero_wrapper() {
    return summCopyZero();
}

int32_t summCopyNonZero_wrapper() {
    return summCopyNonZero();
}

int32_t summCopyZeroDynamic_wrapper() {
    return summCopyZeroDynamic();
}

int32_t summCopyNonZeroDynamic_wrapper() {
    return summCopyNonZeroDynamic();
}

int32_t summViewZero_wrapper() {
    return summViewZero();
}

int32_t summViewNonZero_wrapper() {
    return summViewNonZero();
}

int32_t summViewDynamic_wrapper(const int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return summViewDynamic(arr,/*public*/ __arr_len);
}

int32_t arrget1_wrapper() {
    return arrget1();
}

int32_t arrget2_wrapper() {
    return arrget2();
}

int32_t arrget3_wrapper() {
    return arrget3();
}

int32_t arrget4_wrapper() {
    return arrget4();
}

int32_t arrgetDynamic1_wrapper(const int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return arrgetDynamic1(arr,/*public*/ __arr_len);
}

int32_t arrgetDynamic2_wrapper(const int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return arrgetDynamic2(arr,/*public*/ __arr_len);
}

int32_t arrgetDynamic3_wrapper(const int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return arrgetDynamic3(arr,/*public*/ __arr_len);
}

int32_t arrgetDynamic4_wrapper(const int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return arrgetDynamic4(arr,/*public*/ __arr_len);
}

int32_t mutateRef_wrapper(int32_t * a) {
    return mutateRef(a);
}

int32_t mutateRefCall_wrapper() {
    return mutateRefCall();
}

int32_t simpleArrAccess_wrapper() {
    return simpleArrAccess();
}

int32_t simpleArrZerosAccess_wrapper() {
    return simpleArrZerosAccess();
}

int32_t simpleArrViewAccess_wrapper() {
    return simpleArrViewAccess();
}

int32_t paramArrAccess_wrapper(const int32_t arr[5]) {
    public_in(__SMACK_value(arr));
    return paramArrAccess(arr);
}

int32_t paramArrAccessDyn_wrapper(const int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return paramArrAccessDyn(arr,/*public*/ __arr_len);
}

int32_t paramArrViewAccess_wrapper(const int32_t arr[5]) {
    public_in(__SMACK_value(arr));
    return paramArrViewAccess(arr);
}

int32_t paramArrViewAccessDyn_wrapper(const int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return paramArrViewAccessDyn(arr,/*public*/ __arr_len);
}

int32_t simpleArrayMutation_wrapper(int32_t arr[],/*public*/ uint32_t __arr_len) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    return simpleArrayMutation(arr,/*public*/ __arr_len);
}

int32_t complexArrayMutation_wrapper(int32_t arr[5]) {
    public_in(__SMACK_value(arr));
    return complexArrayMutation(arr);
}

int32_t complexArrayViewMutation_wrapper(int32_t arr[10]) {
    public_in(__SMACK_value(arr));
    return complexArrayViewMutation(arr);
}

int32_t simpleArrayRead_wrapper(int32_t arr[],/*public*/ uint32_t __arr_len,/*public*/ int32_t index) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(__arr_len));
    public_in(__SMACK_value(index));
    return simpleArrayRead(arr,/*public*/ __arr_len,/*public*/ index);
}

int32_t complexArrayViewRead_wrapper(int32_t arr[10],/*public*/ int32_t index) {
    public_in(__SMACK_value(arr));
    public_in(__SMACK_value(index));
    return complexArrayViewRead(arr,/*public*/ index);
}

/*public return*/
int32_t simpleArrCopy_wrapper(int8_t c[],/*public*/ uint32_t __c_len,const int8_t k[5]) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(c));
    public_in(__SMACK_value(__c_len));
    public_in(__SMACK_value(k));
    return simpleArrCopy(c,/*public*/ __c_len,k);
}

/*public return*/
int32_t simpleArrCopy32_wrapper(int32_t c[],/*public*/ uint32_t __c_len,const int32_t k[5]) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(c));
    public_in(__SMACK_value(__c_len));
    public_in(__SMACK_value(k));
    return simpleArrCopy32(c,/*public*/ __c_len,k);
}

/*public return*/
int32_t simpleArrCopyStatic_wrapper(int8_t c[5],const int8_t k[5]) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(c));
    public_in(__SMACK_value(k));
    return simpleArrCopyStatic(c,k);
}

/*public return*/
int32_t simpleArrCopyStatic32_wrapper(int32_t c[5],const int32_t k[5]) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(c));
    public_in(__SMACK_value(k));
    return simpleArrCopyStatic32(c,k);
}

/*public return*/
int32_t arrCopy_wrapper(/*public*/ const int32_t arr[5],/*public*/ int32_t index) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(arr));
    public_in(__SMACK_values(arr,5));
    public_in(__SMACK_value(index));
    return arrCopy(/*public*/ arr,/*public*/ index);
}

/*public return*/
int32_t mediumComplexArrCopy_wrapper(/*public*/ int32_t index) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(index));
    return mediumComplexArrCopy(/*public*/ index);
}

/*public return*/
int32_t complexArrCopy_wrapper(/*public*/ int32_t index) {
    public_out(__SMACK_return_value());
    public_in(__SMACK_value(index));
    return complexArrCopy(/*public*/ index);
}


