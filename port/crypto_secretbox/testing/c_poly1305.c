#include <stdint.h>
#include <string.h>

#define poly1305_block_size 16

/* 17 + sizeof(unsigned long long) + 8*sizeof(unsigned long long) */
typedef struct poly1305_state_internal_t {
    uint64_t r[3];
    uint64_t h[3];
    uint64_t pad[2];
    uint64_t leftover;
    unsigned char      buffer[poly1305_block_size];
    unsigned char      final;
} poly1305_state_internal_t;

#if defined(__SIZEOF_INT128__)
typedef unsigned __int128 uint128_t;
#else
typedef unsigned uint128_t __attribute__((mode(TI)));
#endif

#define MUL(out, x, y) out = ((uint128_t) x * y)
#define ADD(out, in) out += in
#define ADDLO(out, in) out += in
#define SHR(in, shift) (uint64_t) (in >> (shift))
#define LO(in) (uint64_t) (in)

#define LOAD64_LE(SRC) load64_le(SRC)
static inline uint64_t
load64_le(const uint8_t src[8])
{
#ifdef NATIVE_LITTLE_ENDIAN
    uint64_t w;
    memcpy(&w, src, sizeof w);
    return w;
#else
    uint64_t w = (uint64_t) src[0];
    w |= (uint64_t) src[1] <<  8;
    w |= (uint64_t) src[2] << 16;
    w |= (uint64_t) src[3] << 24;
    w |= (uint64_t) src[4] << 32;
    w |= (uint64_t) src[5] << 40;
    w |= (uint64_t) src[6] << 48;
    w |= (uint64_t) src[7] << 56;
    return w;
#endif
}

#define STORE64_LE(DST, W) store64_le((DST), (W))
static inline void
store64_le(uint8_t dst[8], uint64_t w)
{
#ifdef NATIVE_LITTLE_ENDIAN
    memcpy(dst, &w, sizeof w);
#else
    dst[0] = (uint8_t) w; w >>= 8;
    dst[1] = (uint8_t) w; w >>= 8;
    dst[2] = (uint8_t) w; w >>= 8;
    dst[3] = (uint8_t) w; w >>= 8;
    dst[4] = (uint8_t) w; w >>= 8;
    dst[5] = (uint8_t) w; w >>= 8;
    dst[6] = (uint8_t) w; w >>= 8;
    dst[7] = (uint8_t) w;
#endif
}

void
c_poly1305_init(poly1305_state_internal_t *st, const unsigned char key[32])
{
    unsigned long long t0, t1;

    /* r &= 0xffffffc0ffffffc0ffffffc0fffffff */
    t0 = LOAD64_LE(&key[0]);
    t1 = LOAD64_LE(&key[8]);

    /* wiped after finalization */
    st->r[0] = (t0) &0xffc0fffffff;
    st->r[1] = ((t0 >> 44) | (t1 << 20)) & 0xfffffc0ffff;
    st->r[2] = ((t1 >> 24)) & 0x00ffffffc0f;

    /* h = 0 */
    st->h[0] = 0;
    st->h[1] = 0;
    st->h[2] = 0;

    /* save pad for later */
    st->pad[0] = LOAD64_LE(&key[16]);
    st->pad[1] = LOAD64_LE(&key[24]);

    st->leftover = 0;
    st->final    = 0;
}

void
c_poly1305_blocks(poly1305_state_internal_t *st, const unsigned char *m,
                unsigned long long bytes)
{
    const unsigned long long hibit =
        (st->final) ? 0ULL : (1ULL << 40); /* 1 << 128 */
    unsigned long long r0, r1, r2;
    unsigned long long s1, s2;
    unsigned long long h0, h1, h2;
    unsigned long long c;
    uint128_t          d0, d1, d2, d;

    r0 = st->r[0];
    r1 = st->r[1];
    r2 = st->r[2];

    h0 = st->h[0];
    h1 = st->h[1];
    h2 = st->h[2];

    s1 = r1 * (5 << 2);
    s2 = r2 * (5 << 2);

    while (bytes >= poly1305_block_size) {
        unsigned long long t0, t1;

        /* h += m[i] */
        t0 = LOAD64_LE(&m[0]);
        t1 = LOAD64_LE(&m[8]);

        h0 += ((t0) &0xfffffffffff);
        h1 += (((t0 >> 44) | (t1 << 20)) & 0xfffffffffff);
        h2 += (((t1 >> 24)) & 0x3ffffffffff) | hibit;

        /* h *= r */
        MUL(d0, h0, r0);
        MUL(d, h1, s2);
        ADD(d0, d);
        MUL(d, h2, s1);
        ADD(d0, d);
        MUL(d1, h0, r1);
        MUL(d, h1, r0);
        ADD(d1, d);
        MUL(d, h2, s2);
        ADD(d1, d);
        MUL(d2, h0, r2);
        MUL(d, h1, r1);
        ADD(d2, d);
        MUL(d, h2, r0);
        ADD(d2, d);

        /* (partial) h %= p */
        c  = SHR(d0, 44);
        h0 = LO(d0) & 0xfffffffffff;
        ADDLO(d1, c);
        c  = SHR(d1, 44);
        h1 = LO(d1) & 0xfffffffffff;
        ADDLO(d2, c);
        c  = SHR(d2, 42);
        h2 = LO(d2) & 0x3ffffffffff;
        h0 += c * 5;
        c  = (h0 >> 44);
        h0 = h0 & 0xfffffffffff;
        h1 += c;

        m += poly1305_block_size;
        bytes -= poly1305_block_size;
    }

    st->h[0] = h0;
    st->h[1] = h1;
    st->h[2] = h2;
}

void
c_poly1305_finish(poly1305_state_internal_t *st, unsigned char mac[16])
{
    unsigned long long h0, h1, h2, c;
    unsigned long long g0, g1, g2;
    unsigned long long t0, t1;

    /* process the remaining block */
    if (st->leftover) {
        unsigned long long i = st->leftover;

        st->buffer[i] = 1;

        for (i = i + 1; i < poly1305_block_size; i++) {
            st->buffer[i] = 0;
        }
        st->final = 1;
        c_poly1305_blocks(st, st->buffer, poly1305_block_size);
    }

    /* fully carry h */
    h0 = st->h[0];
    h1 = st->h[1];
    h2 = st->h[2];

    c = (h1 >> 44);
    h1 &= 0xfffffffffff;
    h2 += c;
    c = (h2 >> 42);
    h2 &= 0x3ffffffffff;
    h0 += c * 5;
    c = (h0 >> 44);
    h0 &= 0xfffffffffff;
    h1 += c;
    c = (h1 >> 44);
    h1 &= 0xfffffffffff;
    h2 += c;
    c = (h2 >> 42);
    h2 &= 0x3ffffffffff;
    h0 += c * 5;
    c = (h0 >> 44);
    h0 &= 0xfffffffffff;
    h1 += c;

    /* compute h + -p */
    g0 = h0 + 5;
    c  = (g0 >> 44);
    g0 &= 0xfffffffffff;
    g1 = h1 + c;
    c  = (g1 >> 44);
    g1 &= 0xfffffffffff;
    g2 = h2 + c - (1ULL << 42);

    /* select h if h < p, or h + -p if h >= p */
    c = (g2 >> ((sizeof(unsigned long long) * 8) - 1)) - 1;
    g0 &= c;
    g1 &= c;
    g2 &= c;
    c  = ~c;
    h0 = (h0 & c) | g0;
    h1 = (h1 & c) | g1;
    h2 = (h2 & c) | g2;

    /* h = (h + pad) */
    t0 = st->pad[0];
    t1 = st->pad[1];

    h0 += ((t0) &0xfffffffffff);
    c = (h0 >> 44);
    h0 &= 0xfffffffffff;
    h1 += (((t0 >> 44) | (t1 << 20)) & 0xfffffffffff) + c;
    c = (h1 >> 44);
    h1 &= 0xfffffffffff;
    h2 += (((t1 >> 24)) & 0x3ffffffffff) + c;
    h2 &= 0x3ffffffffff;

    /* mac = h % (2^128) */
    h0 = ((h0) | (h1 << 44));
    h1 = ((h1 >> 20) | (h2 << 24));

    STORE64_LE(&mac[0], h0);
    STORE64_LE(&mac[8], h1);

    /* zero out the state */
    memset(st, 0, sizeof *st);
}

void
c_poly1305_update(poly1305_state_internal_t *st, const unsigned char *m,
                unsigned long long bytes)
{
    unsigned long long i;

    /* handle leftover */
    if (st->leftover) {
        unsigned long long want = (poly1305_block_size - st->leftover);

        if (want > bytes) {
            want = bytes;
        }
        for (i = 0; i < want; i++) {
            st->buffer[st->leftover + i] = m[i];
        }
        bytes -= want;
        m += want;
        st->leftover += want;
        if (st->leftover < poly1305_block_size) {
            return;
        }
        c_poly1305_blocks(st, st->buffer, poly1305_block_size);
        st->leftover = 0;
    }

    /* process full blocks */
    if (bytes >= poly1305_block_size) {
        unsigned long long want = (bytes & ~(poly1305_block_size - 1));

        c_poly1305_blocks(st, m, want);
        m += want;
        bytes -= want;
    }

    /* store leftover */
    if (bytes) {
        for (i = 0; i < bytes; i++) {
            st->buffer[st->leftover + i] = m[i];
        }
        st->leftover += bytes;
    }
}

int
c_crypto_onetimeauth_poly1305_donna(unsigned char *out, const unsigned char *m,
                                  unsigned long long   inlen,
                                  const unsigned char *key)
{
    poly1305_state_internal_t state;

    c_poly1305_init(&state, key);
    c_poly1305_update(&state, m, inlen);
    c_poly1305_finish(&state, out);

    return 0;
}
