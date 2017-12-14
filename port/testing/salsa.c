#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "fact_salsa.h"
void
c_crypto_core_salsa(unsigned char *out, const unsigned char *in,
                  const unsigned char *k, const unsigned char *c,
                  const int rounds);

static unsigned char key[32] = { 0x1b, 0x27, 0x55, 0x64, 0x73, 0xe9, 0x85, 0xd4,
                                 0x62, 0xcd, 0x51, 0x19, 0x7a, 0x9a, 0x46, 0xc7,
                                 0x60, 0x09, 0x54, 0x9e, 0xac, 0x64, 0x74, 0xf2,
                                 0x06, 0xc4, 0xee, 0x08, 0x44, 0xf6, 0x83, 0x89 };

static unsigned char in[16] = { 0x69, 0x69, 0x6e, 0xe9, 0x55, 0xb6, 0x2b, 0x73,
                                0xcd, 0x62, 0xbd, 0xa8, 0x75, 0xfc, 0x73, 0xd6 };

static unsigned char out[64];

int main(void)
{
    int i;

    memset(out, 0, 64);
    c_crypto_core_salsa(out, in, key, NULL, 20);
    for (i = 0; i < 64; ++i) {
        printf(",0x%02x", (unsigned int) out[i]);
        if (i % 8 == 7)
            printf("\n");
    }
    printf("\n");

    memset(out, 0, 64);
    fact_crypto_core_salsa20(out, in, key, 1);
    for (i = 0; i < 64; ++i) {
        printf(",0x%02x", (unsigned int) out[i]);
        if (i % 8 == 7)
            printf("\n");
    }
    printf("\n");

    return 0;
}
