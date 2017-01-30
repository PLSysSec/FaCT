#ifndef OPENSSL_H
#define OPENSSL_H

#define MAX_HASH_BLOCK_SIZE 128

typedef struct ssl3_record_st {
    unsigned int length;
    unsigned char *data;    
    int type;
    unsigned char *input;
} SSL3_RECORD;

typedef struct ssl3_state_st {
    long flags;
    unsigned char read_sequence[8];
} SSL3_STATE;

typedef struct evp_cipher_st {
    unsigned long flags;
} EVP_CIPHER;

typedef struct evp_cipher_ctx_st {
    const EVP_CIPHER *cipher;
} EVP_CIPHER_CTX;

typedef struct ssl_st {
    char *expand;
    unsigned long options;
    struct ssl3_state_st *s3;   
    EVP_CIPHER_CTX *enc_read_ctx; 
    int slicing_cheat;
} SSL;

# define SSL_OP_TLS_BLOCK_PADDING_BUG       0x00000200L
# define TLS1_FLAGS_TLS_PADDING_BUG         0x0008
# define SSL_USE_EXPLICIT_IV(a) (a->slicing_cheat&1) // slicing
# define EVP_CIPHER_flags(e)        ((e)->flags)
# define EVP_CIPH_FLAG_AEAD_CIPHER       0x200000
# define EVP_MAX_MD_SIZE                 64/* longest known is SHA512 */
# define OPENSSL_assert(a) 1; // slicing


/******** Benchmarking code below ********/
// TODO: Initialize all of these appropriately
#define ROUTINE_DEC \
    SSL s_obj; \
    const SSL *s = &s_obj; \
    SSL3_RECORD rec_obj; \
    unsigned int length = 128; \
    unsigned char *data = calloc (length, sizeof(unsigned char)); \
    unsigned block_size = 32; \
    unsigned mac_size = 16;

#define ROUTINE_INIT \
    s_obj = (SSL) { NULL, 0, NULL, NULL, 0}; \
    rec_obj = (SSL3_RECORD) { length, data, 0, NULL };

#define ROUTINE ssl3_cbc_remove_padding(s,&rec_obj,block_size,mac_size);
int ssl3_cbc_remove_padding(const SSL *s, SSL3_RECORD *rec, unsigned block_size, unsigned mac_size);


//TODO: Fix this
#define BARRIER_DATA __asm__ __volatile__("# barrier": :"r"(s) :"memory");

#endif

