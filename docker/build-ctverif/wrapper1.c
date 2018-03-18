#include "/root/verifying-constant-time/examples/ct-verif.h"
#include <stdlib.h>

void remove_secret_padding(uint8_t *p, uint32_t public_size, uint32_t secret_len);

void wrapper(uint8_t *p, uint32_t public_size, uint32_t secret_len) {
        public_in(__SMACK_value(p));
        public_in(__SMACK_value(public_size));
        remove_secret_padding(p,public_size,secret_len);
}
