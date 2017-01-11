#include <smack.h>
#include "../ct-verif.h"

int ssl3_cbc_remove_padding(int* data, int* input, int block_size, int mac_size, int* lengthtype_array);

int ssl3_cbc_remove_padding_wrapper(int* data, int length, int block_size, int mac_size) {

  // pointer _values_ are public, maybe not the contents
  public_in(__SMACK_value(data));

  // these lengths are all public
  public_in(__SMACK_value(length));
  public_in(__SMACK_value(block_size));
  public_in(__SMACK_value(mac_size));

  int lengthtype_array[2] = {length,0};
  int * input = 0;
  return ssl3_cbc_remove_padding(data, input, block_size, mac_size, lengthtype_array);
}
