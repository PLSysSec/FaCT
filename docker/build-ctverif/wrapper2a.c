#include "/root/verifying-constant-time/examples/ct-verif.h"
#include <stdlib.h>
#include <stdint.h>

int check_pkcs7_padding(const uint8_t *buf, uint32_t public_size);

int wrapper(const uint8_t *buf, uint32_t public_size) {
        public_in(__SMACK_value(buf));
        public_in(__SMACK_value(public_size));
        return check_pkcs7_padding(buf,public_size);
}
