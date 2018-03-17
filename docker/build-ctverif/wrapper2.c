#include "/root/verifying-constant-time/examples/ct-verif.h"
#include <stdlib.h>

int remove_pkcs7_padding(unsigned char *buf, size_t public_size);

void wrapper(unsigned char *buf, size_t public_size) {
        public_in(__SMACK_value(buf));
        public_in(__SMACK_value(public_size));
        remove_pkcs7_padding(buf,public_size);
}
