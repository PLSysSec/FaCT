#include "internal/constant_time_locl.h"
#include "ssl_locl.h"
#include "internal/cryptlib.h"

#include <openssl/md5.h>
#include <openssl/sha.h>
            void _md_transform(int ctx_type, void* ctx, const unsigned char *block) {
              switch (ctx_type) {
    case NID_sha1:
      //printf("md sha1\n");
              SHA1_Transform(ctx, block);
              break;
    case NID_sha256:
      //printf("md sha256\n");
              SHA256_Transform(ctx, block);
              break;
    case NID_sha384:
      //printf("md sha384\n");
              SHA512_Transform(ctx, block);
              break;
    default:
              ((char*)ctx)[88888888888888] = 4;
              break;
              }
            }
static void tls1_sha1_final_raw(void *ctx, unsigned char *md_out)
{
    SHA_CTX *sha1 = ctx;
    l2n(sha1->h0, md_out);
    l2n(sha1->h1, md_out);
    l2n(sha1->h2, md_out);
    l2n(sha1->h3, md_out);
    l2n(sha1->h4, md_out);
}
static void tls1_sha256_final_raw(void *ctx, unsigned char *md_out)
{
    SHA256_CTX *sha256 = ctx;
    unsigned i;

    for (i = 0; i < 8; i++) {
        l2n(sha256->h[i], md_out);
    }
}
static void tls1_sha512_final_raw(void *ctx, unsigned char *md_out)
{
    SHA512_CTX *sha512 = ctx;
    unsigned i;

    for (i = 0; i < 8; i++) {
        l2n8(sha512->h[i], md_out);
    }
}
        void _md_final_raw(int ctx_type, void* ctx, unsigned char *md_out) {
          switch (ctx_type) {
    case NID_sha1:
          tls1_sha1_final_raw(ctx, md_out);
          break;
    case NID_sha256:
          tls1_sha256_final_raw(ctx, md_out);
          break;
    case NID_sha384:
          tls1_sha512_final_raw(ctx, md_out);
          break;
    default:
              ((char*)ctx)[88888888888888] = 4;
              break;
          }
        }

void _print(uint64_t n) {
  //printf("_print: %lu\n", n);
}
void _prints(uint64_t n) {
  //printf("%lu ", n);
}
void _println() {
  //printf("\n");
}
void _printx(uint8_t b) {
  //printf("%02x ", b);
}
