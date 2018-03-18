#include "/root/verifying-constant-time/examples/ct-verif.h"
#include <stdlib.h>

uint32_t sum_array(const uint8_t *arr, uint32_t arr_len);

void wrapper(const uint8_t *arr, uint32_t arr_len) {
        public_in(__SMACK_value(arr));
        public_in(__SMACK_value(arr_len));
        sum_array(arr,arr_len);
}
