#include "Unity/src/unity.h"
#include <stdlib.h>

int ssl3_cbc_remove_padding(int,char*,int,char*,int,int);

void test_openssl(void){
  int length = 1;
  char arr[5] = {0,0,0,0,0};
  int type = 1;
  char input[5] = {0,0,0,0,0};
  int block_size = 1;
  int mac_size = 1;
  ssl3_cbc_remove_padding(length, arr, type, input, block_size, mac_size);
}

int main(void) {
  UNITY_BEGIN();
  RUN_TEST(test_openssl);
  return UNITY_END();
}
