#include "Unity/src/unity.h"
#include <stdlib.h>

int ssl3_cbc_remove_padding(char*,char*,int,int,char*);
[{name="data"; ty=(ByteArr 1024)};
{name="input"; ty=(ByteArr 1024)}; {name="block_size"; ty=Int};
{name="mac_size"; ty=Int}; {name="lengthtype_array"; ty=(ByteArr 2)}], Int,
void test_openssl(void){
  char data[5] = {0,0,0,0,0};
  char input[5] = {0,0,0,0,0};
  int block_size = 1;
  int mac_size = 1;
  char lengthtype_array[2] = {0,0};
  ssl3_cbc_remove_padding(data, input, block_size, mac_size, lengthtype_array);
}

int main(void) {
  UNITY_BEGIN();
  RUN_TEST(test_openssl);
  return UNITY_END();
}
