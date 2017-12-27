#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "fact_poly1305.h"

#define BYTES_OF_INPUT 131

#define poly1305_block_size 32

/* 17 + sizeof(unsigned long long) + 8*sizeof(unsigned long long) */
typedef struct poly1305_state_internal_t {
    uint64_t r[3];
    uint64_t h[3];
    uint64_t pad[2];
    uint64_t leftover;
    unsigned char      buffer[poly1305_block_size];
    unsigned char      final;
} poly1305_state_internal_t;

void
c_poly1305_init(poly1305_state_internal_t *st, const unsigned char key[32]);
void
c_poly1305_blocks(poly1305_state_internal_t *st, const unsigned char *m,
                unsigned long long bytes);
void
c_poly1305_update(poly1305_state_internal_t *st, const unsigned char *m,
                unsigned long long bytes);
void
c_poly1305_finish(poly1305_state_internal_t *st, unsigned char mac[16]);
int
c_crypto_onetimeauth_poly1305_donna(unsigned char *out, const unsigned char *m,
                                  unsigned long long   inlen,
                                  const unsigned char *key);

static unsigned char key[32] = { 0x1b, 0x27, 0x55, 0x64, 0x73, 0xe9, 0x85, 0xd4,
                                 0x62, 0xcd, 0x51, 0x19, 0x7a, 0x9a, 0x46, 0xc7,
                                 0x60, 0x09, 0x54, 0x9e, 0xac, 0x64, 0x74, 0xf2,
                                 0x06, 0xc4, 0xee, 0x08, 0x44, 0xf6, 0x83, 0x89 };

static unsigned char in[131] = {
    0xbe, 0x07, 0x5f, 0xc5, 0x3c, 0x81, 0xf2, 0xd5,
    0xcf, 0x14, 0x13, 0x16, 0xeb, 0xeb, 0x0c, 0x7b,
    0x52, 0x28, 0xc5, 0x2a, 0x4c, 0x62, 0xcb, 0xd4,
    0x4b, 0x66, 0x84, 0x9b, 0x64, 0x24, 0x4f, 0xfc,
    0xe5, 0xec, 0xba, 0xaf, 0x33, 0xbd, 0x75, 0x1a,
    0x1a, 0xc7, 0x28, 0xd4, 0x5e, 0x6c, 0x61, 0x29,
    0x6c, 0xdc, 0x3c, 0x01, 0x23, 0x35, 0x61, 0xf4,
    0x1d, 0xb6, 0x6c, 0xce, 0x31, 0x4a, 0xdb, 0x31,
    0x0e, 0x3b, 0xe8, 0x25, 0x0c, 0x46, 0xf0, 0x6d,
    0xce, 0xea, 0x3a, 0x7f, 0xa1, 0x34, 0x80, 0x57,
    0xe2, 0xf6, 0x55, 0x6a, 0xd6, 0xb1, 0x31, 0x8a,
    0x02, 0x4a, 0x83, 0x8f, 0x21, 0xaf, 0x1f, 0xde,
    0x04, 0x89, 0x77, 0xeb, 0x48, 0xf5, 0x9f, 0xfd,
    0x49, 0x24, 0xca, 0x1c, 0x60, 0x90, 0x2e, 0x52,
    0xf0, 0xa0, 0x89, 0xbc, 0x76, 0x89, 0x70, 0x40,
    0xe0, 0x82, 0xf9, 0x37, 0x76, 0x38, 0x48, 0x64,
    0x5e, 0x07, 0x05
};

static unsigned char out[16];

void print64(uint64_t n)
{
    uint8_t *c = (uint8_t*)&n;
    c += 7;
    printf("    ");
    for (int i = 0; i < 8; i++)
        printf("%02x ", *c--);
    printf("\n");
}

void print_state(poly1305_state_internal_t* st)
{
    printf("r\n");
    for (int i = 0; i < 3; i++)
        print64(st->r[i]);
    printf("h\n");
    for (int i = 0; i < 3; i++)
        print64(st->h[i]);
    printf("pad\n");
    for (int i = 0; i < 2; i++)
        print64(st->pad[i]);
    printf("leftover\n");
    print64(st->leftover);
    printf("buffer and final\n");
    printf("    ");
    for (int i = 0; i < poly1305_block_size; i++)
        printf("%02x ", st->buffer[i]);
    printf("\n    %02x\n", st->final);
}

int main(void)
{
    int i;
    poly1305_state_internal_t st;

    printf("C:\n");
    memset(&st, 0, sizeof(poly1305_state_internal_t));
    memset(out, 0, 16);
    //c_poly1305_init(&st, key);
    //c_poly1305_update(&st, in, 32);
    //c_poly1305_finish(&st, out);
    //print_state(&st);
    c_crypto_onetimeauth_poly1305_donna(out, in, BYTES_OF_INPUT, key);
    for (int i = 0; i < 16; i++)
    {
        printf("%02x ", out[i]);
        if (i % 8 == 7)
            printf("\n");
    }
    printf("\n");

    printf("FaCT:\n");
    memset(&st, 0, sizeof(poly1305_state_internal_t));
    memset(out, 0, 16);
    //fact_poly1305_init(st.r, st.h, st.pad, &st.leftover, st.buffer, &st.final, key, 1);
    //fact_poly1305_update(st.r, st.h, st.pad, &st.leftover, st.buffer, &st.final, in, 32, 1);
    //fact_poly1305_finish(st.r, st.h, st.pad, &st.leftover, st.buffer, &st.final, out, 1);
    //print_state(&st);
    fact_crypto_onetimeauth_poly1305(out, in, BYTES_OF_INPUT, key, 1);
    for (int i = 0; i < 16; i++)
    {
        printf("%02x ", out[i]);
        if (i % 8 == 7)
            printf("\n");
    }
    printf("\n");

    return 0;
}
