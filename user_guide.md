# Constanc User Guide

### Table of Contents
1. [Types](#typese)
    1. [Number](#number)
    2. [Boolean](#boolean)
    3. [Byte Array](#byte-array)
2. [Functions](#functions)
    1. [Return Value](#return-value)
    2. [Arguments](#arguments)
    3. [Function Calls](#function-calls)
3. [Labels](#labels)
4. [Operations](#operations)
    1. [Binary Operations](#binary-operations)
    2. [Unary Operations](#unary-operations)
    3. [Assignment Operations](#assignment-operations)
5. [If-Statement](#if-statement)
6. [Loops](#loops)
7. [Full Working Example](#full-working-example)

## Types

The constanc type system has 3 types: numbers, booleans, and byte arrays.

### Number

Numbers can be decimal, hex, or binary

Example Decimal:
```C
int someNumber = 1;
```

Example Hex:
```C
int someNumber = 0x123a;
```

Example Binary:
```C
int someNumber = 0b1101;
```

Please note that all numbers are 32 bits. We do not support 64 bits yet.


### Boolean

Example:
```C
bool someBool = true;
```

Example:
```C
bool someBool = false;
```

### Byte Array

A byte array is an array that stores 32 bit values.

Example create:
```C
bytearr[5] br = [1,2,0x111,0b010101,5];
```

Example Access:
```C
int accessedValue = br[1];
```

Example Set:
```C
br[0] = 100;
```

Please note, byte array values can only be accessed using a public value. We will go over public and secret labels later in the guide.

## Functions

Functions have a return label, return type, and any number of arguments. 

Example:
```C
int one(){
  return 1;
}
```

### Return Value

The return value of a function is specified using the ```return``` keyword. Constanc supports early return, so multiple return statements are allowed. 

Functions cannot return byte arrays. If you are calling a constanc function from C and need to set values in a byte array, then it must be passed in as an argument. Constanc will not manage heap allocated objects passed in as an argument.

### Arguments

Functions support any number of arguments. The type must be specified, but the label can be left ambiguous. Argument labels behave differently than other values in that they will never be ambiguous. If an argument label is unspecified, it is set to be ```secret```.

Example:
```C
int add5(int num) {
  return num + 5;
}

```

Example:
```C
int add5(public int num) {
  return num + 5;
}

```

In the second example, since the argument is set to ```public```, the return value is also ```public```. In the first example, the argument is automatically set to ```secret```, so the the return value is inferred to be ```secret```.

### Function Calls

Constanc uses C-style function calls.

Example:
```C
int a = add5(0);
```

## Labels

Labels are used to describe the sensitivity/secrecy of data. There are two types of labels: secret and public. All functions and values have an associated label, although it is not required to specify it. Since it is not required, constanc uses a label inference algorithm to fill in the blanks. An ambiguous label is one which is unspecified and has not yet been assigned secret or public.

The typechecker will update all ambiguous labels to be either public or secret. If the typechecker is unable to assign a value a label at the end of typechecking, the label is updated to secret. Also, function argument labels are never inferred. If left ambiguous, they are immediately updated to secret.


Examples:
```C
secret int add5(secret int num) {
  return num + 5;
}

secret int add(secret int a, public int b) {
  return a + b;
}

// Note the label of secretValue is `secret`.
// Therefore, the label of the function is inferred to be `secret` (because a 
// secret value is flowing into an ambiguous value)
int add2(int secretValue) {
  return secretValue + 2;
}
```

```C
int a = 1; // ambiguous label
public int b = 1; // public label
secret int c = 1; // secret label
```

## Operations

### Binary Operations

All binary operations have infix syntax. The complete list of operations is below.

| Operation                | Operator |
|--------------------------|----------|
| Addition                 | +        |
| Subtraction              | -        |
| Multiplication           | *        |
| Equals                   | ==       |
| Not Equal                | !=       |
| Greater Than             | >        |
| Greater Than Or Equal To | >=       |
| Less Than                | <        |
| Less Than Or Equal To    | <=       |
| Bitwise And              | &        |
| Bitwise Or               | \|       |
| Leftshift                | <<       |
| Rightshift               | >>       |


### Unary Operations

All unary operations have prefix syntax. The complete list of operations is below.

| Operation                | Operator |
|--------------------------|----------|
| Bitwise Not              | ~        |
| Logiacal Not             | !        |
| Negation                 | -        |


### Assignment Operations

Many assignment operations are supported. The complete list is below

| Operation                | Operator |
|--------------------------|----------|
| Bitwise AND              | &=       |
| Bitwise XOR              | ^=       |
| Bitwise OR               | \|=      |
| Leftshift                | <<=      |
| Rightshift               | >>=      |
| Addition                 | +=       |
| Subtraction              | -=       |
| Multiplication           | *=       |

An example usage is below.

```C
int a = 1;
a += 1;
```


## if-statement

If-then-else statements are supported. Unfortunately, no other variations of the if-statement is(ex. else-if). An example is below.

Example:
```C

if(a < 10) {
  return 1;
} else {
  return 2;
}
```

## Loops

Constanc supports for loops. You must specify the low and high bounds. The low bound is inclusive while the high bound is exclusive. The bounds must be literals. The example below iterates over a byte array and sets each value to 0. 

```C
bytearr[10] br = [1,2,3,4,5,6,7,8,9,10];
for(i=0 to 10) {
  br[i] = 0;
}
```

## Full Working Example

Constanc code is designed to be called from C code. This example shows how to do this using gcc, but it can also be done using clang.

```C
// main.c

#include <stdlib.h>
#include <stdio.h>

int mutateArray(int*,int);

int printArray(int* myArr) {
  for(int i=0; i < 5; i++) {
    printf("%d: %d\n", i, myArr[i]);
  }
}

int main() {
  int myArray[5] = {1,2,3,4,5};
  mutateArray(myArray, 0);
  printArray(myArray);
  mutateArray(myArray, 1);
  printArray(myArray);
  return 1;
}
```

```C
// mutate.const

int add5(int num) {
  return num + 5;
}

int mutateArray(secret bytearr[5] arr, bool mutate) {
  if(mutate) {
    for(i=0 to 5) {
      arr[i] = add5(arr[i]);
    }
  } else {
    return 1;
  }
  return 1;
}

```

The code above can be compiled and run with the following commands.

1. ```constanc mutate.const```
2. ```gcc -c main.c```
3. ```gcc -o final main.o mutate.o```
4. ```./final```