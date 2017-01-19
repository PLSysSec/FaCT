#include "constant_time_locl.h"
#include <string.h>

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

/*-
 * ssl3_cbc_remove_padding removes padding from the decrypted, SSLv3, CBC
 * record in |rec| by updating |rec->length| in constant time.
 *
 * block_size: the block size of the cipher used to encrypt the record.
 * returns:
 *   0: (in non-constant time) if the record is publicly invalid.
 *   1: if the padding was valid
 *  -1: otherwise.
 */
int ssl3_cbc_remove_padding(const SSL *s,
                            SSL3_RECORD *rec,
                            unsigned block_size, unsigned mac_size)
{
    unsigned padding_length, good;
    const unsigned overhead = 1 /* padding length byte */  + mac_size;

    /*
     * These lengths are all public so we can test them in non-constant time.
     */
    if (overhead > rec->length)
        return 0;
    
    padding_length = rec->data[rec->length - 1];
    good = constant_time_ge(rec->length, padding_length + overhead);
    /* SSLv3 requires that the padding is minimal. */
    good &= constant_time_ge(block_size, padding_length + 1);
    padding_length = good & (padding_length + 1);
    rec->length -= padding_length;
    rec->type |= padding_length << 8; /* kludge: pass padding length */
    return constant_time_select_int(good, 1, -1);
}
