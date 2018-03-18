#include "/root/verifying-constant-time/examples/ct-verif.h"
#include <stdlib.h>
#include <stdint.h>

uint32_t sum_array(uint8_t *arr, uint32_t arr_len);

void wrapper(uint8_t *arr, uint32_t arr_len) {
        public_in(__SMACK_value(arr));
        public_in(__SMACK_value(arr_len));
        public_out(__SMACK_return_value());
        sum_array(arr,arr_len);
}
