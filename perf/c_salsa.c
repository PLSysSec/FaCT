#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define NATIVE_LITTLE_ENDIAN

static
void sodium_memzero(void* buf, size_t len)
{
    memset(buf, 0, len);
}

#define LOAD32_LE(SRC) load32_le(SRC)
static inline uint32_t
load32_le(const uint8_t src[4])
{
#ifdef NATIVE_LITTLE_ENDIAN
    uint32_t w;
    memcpy(&w, src, sizeof w);
    return w;
#else
    uint32_t w = (uint32_t) src[0];
    w |= (uint32_t) src[1] <<  8;
    w |= (uint32_t) src[2] << 16;
    w |= (uint32_t) src[3] << 24;
    return w;
#endif
}

#define STORE32_LE(DST, W) store32_le((DST), (W))
static inline void
store32_le(uint8_t dst[4], uint32_t w)
{
#ifdef NATIVE_LITTLE_ENDIAN
    memcpy(dst, &w, sizeof w);
#else
    dst[0] = (uint8_t) w; w >>= 8;
    dst[1] = (uint8_t) w; w >>= 8;
    dst[2] = (uint8_t) w; w >>= 8;
    dst[3] = (uint8_t) w;
#endif
}

#define ROTL32(X, B) rotl32((X), (B))
static inline uint32_t
rotl32(const uint32_t x, const int b)
{
    return (x << b) | (x >> (32 - b));
}

#define ROUNDS 20
#define U32C(v) (v##U)

uint32_t rotcore(unsigned char *in) {
  uint32_t x0 = LOAD32_LE(in + 0);
  uint32_t x4 = LOAD32_LE(in + 4);
  uint32_t x8 = LOAD32_LE(in + 8);
  uint32_t x12 = LOAD32_LE(in + 12);
  for (int i = 0; i < 10; i++) {
    x4  ^= ROTL32(x0  + x12, 7);
    x8  ^= ROTL32(x4  + x0, 9);
    x12 ^= ROTL32(x8  + x4, 13);
    x0  ^= ROTL32(x12 + x8, 18);
  }
  return x0;
}

void
crypto_core_salsa20(unsigned char *out, const unsigned char *in,
                  const unsigned char *k)
{
    uint32_t x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15;
    uint32_t j0, j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12, j13, j14, j15;
    int i;
    int rounds = ROUNDS;

    j0  = x0  = 0x61707865;
    j5  = x5  = 0x3320646e;
    j10 = x10 = 0x79622d32;
    j15 = x15 = 0x6b206574;

    j1  = x1  = LOAD32_LE(k + 0);
    j2  = x2  = LOAD32_LE(k + 4);
    j3  = x3  = LOAD32_LE(k + 8);
    j4  = x4  = LOAD32_LE(k + 12);
    j11 = x11 = LOAD32_LE(k + 16);
    j12 = x12 = LOAD32_LE(k + 20);
    j13 = x13 = LOAD32_LE(k + 24);
    j14 = x14 = LOAD32_LE(k + 28);

    j6  = x6  = LOAD32_LE(in + 0);
    j7  = x7  = LOAD32_LE(in + 4);
    j8  = x8  = LOAD32_LE(in + 8);
    j9  = x9  = LOAD32_LE(in + 12);

    for (i = 0; i < rounds; i += 2) {
        x4  ^= ROTL32(x0  + x12, 7);
        x8  ^= ROTL32(x4  + x0, 9);
        x12 ^= ROTL32(x8  + x4, 13);
        x0  ^= ROTL32(x12 + x8, 18);
        x9  ^= ROTL32(x5  + x1, 7);
        x13 ^= ROTL32(x9  + x5, 9);
        x1  ^= ROTL32(x13 + x9, 13);
        x5  ^= ROTL32(x1  + x13, 18);
        x14 ^= ROTL32(x10 + x6, 7);
        x2  ^= ROTL32(x14 + x10, 9);
        x6  ^= ROTL32(x2  + x14, 13);
        x10 ^= ROTL32(x6  + x2, 18);
        x3  ^= ROTL32(x15 + x11, 7);
        x7  ^= ROTL32(x3  + x15, 9);
        x11 ^= ROTL32(x7  + x3, 13);
        x15 ^= ROTL32(x11 + x7, 18);
        x1  ^= ROTL32(x0  + x3, 7);
        x2  ^= ROTL32(x1  + x0, 9);
        x3  ^= ROTL32(x2  + x1, 13);
        x0  ^= ROTL32(x3  + x2, 18);
        x6  ^= ROTL32(x5  + x4, 7);
        x7  ^= ROTL32(x6  + x5, 9);
        x4  ^= ROTL32(x7  + x6, 13);
        x5  ^= ROTL32(x4  + x7, 18);
        x11 ^= ROTL32(x10 + x9, 7);
        x8  ^= ROTL32(x11 + x10, 9);
        x9  ^= ROTL32(x8  + x11, 13);
        x10 ^= ROTL32(x9  + x8, 18);
        x12 ^= ROTL32(x15 + x14, 7);
        x13 ^= ROTL32(x12 + x15, 9);
        x14 ^= ROTL32(x13 + x12, 13);
        x15 ^= ROTL32(x14 + x13, 18);
    }
    STORE32_LE(out + 0,  x0  + j0);
    STORE32_LE(out + 4,  x1  + j1);
    STORE32_LE(out + 8,  x2  + j2);
    STORE32_LE(out + 12, x3  + j3);
    STORE32_LE(out + 16, x4  + j4);
    STORE32_LE(out + 20, x5  + j5);
    STORE32_LE(out + 24, x6  + j6);
    STORE32_LE(out + 28, x7  + j7);
    STORE32_LE(out + 32, x8  + j8);
    STORE32_LE(out + 36, x9  + j9);
    STORE32_LE(out + 40, x10 + j10);
    STORE32_LE(out + 44, x11 + j11);
    STORE32_LE(out + 48, x12 + j12);
    STORE32_LE(out + 52, x13 + j13);
    STORE32_LE(out + 56, x14 + j14);
    STORE32_LE(out + 60, x15 + j15);
}

int
crypto_core_hsalsa20(unsigned char *out,
                     const unsigned char *in,
                     const unsigned char *k)
{
    uint32_t x0, x1, x2, x3, x4, x5, x6, x7, x8,
             x9, x10, x11, x12, x13, x14,  x15;
    int      i;

    x0 = U32C(0x61707865);
    x5 = U32C(0x3320646e);
    x10 = U32C(0x79622d32);
    x15 = U32C(0x6b206574);

    x1 = LOAD32_LE(k + 0);
    x2 = LOAD32_LE(k + 4);
    x3 = LOAD32_LE(k + 8);
    x4 = LOAD32_LE(k + 12);
    x11 = LOAD32_LE(k + 16);
    x12 = LOAD32_LE(k + 20);
    x13 = LOAD32_LE(k + 24);
    x14 = LOAD32_LE(k + 28);
    x6 = LOAD32_LE(in + 0);
    x7 = LOAD32_LE(in + 4);
    x8 = LOAD32_LE(in + 8);
    x9 = LOAD32_LE(in + 12);

    for (i = ROUNDS; i > 0; i -= 2) {
        x4 ^= ROTL32(x0 + x12, 7);
        x8 ^= ROTL32(x4 + x0, 9);
        x12 ^= ROTL32(x8 + x4, 13);
        x0 ^= ROTL32(x12 + x8, 18);
        x9 ^= ROTL32(x5 + x1, 7);
        x13 ^= ROTL32(x9 + x5, 9);
        x1 ^= ROTL32(x13 + x9, 13);
        x5 ^= ROTL32(x1 + x13, 18);
        x14 ^= ROTL32(x10 + x6, 7);
        x2 ^= ROTL32(x14 + x10, 9);
        x6 ^= ROTL32(x2 + x14, 13);
        x10 ^= ROTL32(x6 + x2, 18);
        x3 ^= ROTL32(x15 + x11, 7);
        x7 ^= ROTL32(x3 + x15, 9);
        x11 ^= ROTL32(x7 + x3, 13);
        x15 ^= ROTL32(x11 + x7, 18);
        x1 ^= ROTL32(x0 + x3, 7);
        x2 ^= ROTL32(x1 + x0, 9);
        x3 ^= ROTL32(x2 + x1, 13);
        x0 ^= ROTL32(x3 + x2, 18);
        x6 ^= ROTL32(x5 + x4, 7);
        x7 ^= ROTL32(x6 + x5, 9);
        x4 ^= ROTL32(x7 + x6, 13);
        x5 ^= ROTL32(x4 + x7, 18);
        x11 ^= ROTL32(x10 + x9, 7);
        x8 ^= ROTL32(x11 + x10, 9);
        x9 ^= ROTL32(x8 + x11, 13);
        x10 ^= ROTL32(x9 + x8, 18);
        x12 ^= ROTL32(x15 + x14, 7);
        x13 ^= ROTL32(x12 + x15, 9);
        x14 ^= ROTL32(x13 + x12, 13);
        x15 ^= ROTL32(x14 + x13, 18);
    }

    STORE32_LE(out + 0, x0);
    STORE32_LE(out + 4, x5);
    STORE32_LE(out + 8, x10);
    STORE32_LE(out + 12, x15);
    STORE32_LE(out + 16, x6);
    STORE32_LE(out + 20, x7);
    STORE32_LE(out + 24, x8);
    STORE32_LE(out + 28, x9);

    return 0;
}

int
crypto_stream_salsa20_xor_ic(unsigned char *c, unsigned long long clen, const unsigned char *m,
                  unsigned long long mlen, const unsigned char *n, uint64_t ic,
                  const unsigned char *k)
{
    unsigned char in[16];
    unsigned char block[64];
    unsigned char kcopy[32];
    unsigned int  i;
    unsigned int  u;

    if (!mlen) {
        return 0;
    }
    for (i = 0; i < 32; i++) {
        kcopy[i] = k[i];
    }
    for (i = 0; i < 8; i++) {
        in[i] = n[i];
    }
    for (i = 8; i < 16; i++) {
        in[i] = (unsigned char) (ic & 0xff);
        ic >>= 8;
    }
    while (mlen >= 64) {
        crypto_core_salsa20(block, in, kcopy);
        for (i = 0; i < 64; i++) {
            c[i] = m[i] ^ block[i];
        }
        u = 1;
        for (i = 8; i < 16; i++) {
            u += (unsigned int) in[i];
            in[i] = u;
            u >>= 8;
        }
        mlen -= 64;
        c += 64;
        m += 64;
    }
    if (mlen) {
        crypto_core_salsa20(block, in, kcopy);
        for (i = 0; i < (unsigned int) mlen; i++) {
            c[i] = m[i] ^ block[i];
        }
    }
    sodium_memzero(block, sizeof block);
    sodium_memzero(kcopy, sizeof kcopy);

    return 0;
}
