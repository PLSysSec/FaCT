#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "fact_salsa.h"

static unsigned char subkey[32] = { 0x1b, 0x27, 0x55, 0x64, 0x73, 0xe9, 0x85, 0xd4,
                                 0x62, 0xcd, 0x51, 0x19, 0x7a, 0x9a, 0x46, 0xc7,
                                 0x60, 0x09, 0x54, 0x9e, 0xac, 0x64, 0x74, 0xf2,
                                 0x06, 0xc4, 0xee, 0x08, 0x44, 0xf6, 0x83, 0x89 };

static unsigned char out_fact[168];

void compare_outputs(int nlines)
{
    for (int i = 0; i < nlines; i++)
    {
        for (int j = 0; j < 8; j++)
        {
            int n = i*8 + j;
            printf("%02x ", subkey[n]);
        }
        printf("  ");
        for (int j = 0; j < 8; j++)
        {
            int n = i*8 + j;
            printf("%02x ", out_fact[n]);
        }
        char res = 1;
        for (int j = 0; j < 8; j++)
        {
            int n = i*8 + j;
            if (subkey[n] != out_fact[n])
            {
                res = 0;
                break;
            }
        }
        printf("  %s\n", res ? "OK" : "NOPE");
    }
    printf("\n");
}

int main(void)
{
    bad(out_fact, 64, subkey, 1);
    compare_outputs(4);

    return 0;
}
