#include "/root/verifying-constant-time/examples/ct-verif.h"
#include <stdlib.h>
#include <stdint.h>

int remove_pkcs7_padding(uint8_t *buf, uint32_t public_size);

void wrapper(uint8_t *buf, uint32_t public_size) {
        public_in(__SMACK_value(buf));
        public_in(__SMACK_value(public_size));
        remove_pkcs7_padding(buf,public_size);
}
