/*********************************************************************
* Filename:   aes.h
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Defines the API for the corresponding AES implementation.
*********************************************************************/

#ifndef AES_H
#define AES_H

/*************************** HEADER FILES ***************************/
#include <stddef.h>

/****************************** MACROS ******************************/
#define AES_BLOCK_SIZE 16               // AES operates on 16 bytes at a time

/**************************** DATA TYPES ****************************/
typedef unsigned char BYTE;            // 8-bit byte
typedef unsigned int WORD;             // 32-bit word, change to "long" for 16-bit machines

/*********************** FUNCTION DECLARATIONS **********************/
///////////////////
// AES
///////////////////
// Key setup must be done before any AES en/de-cryption functions can be used.
void aes_key_setup(const BYTE key[],          // The key, must be 128, 192, or 256 bits
                   WORD w[],                  // Output key schedule to be used later
                   int keysize,              // Bit length of the key, 128, 192, or 256
                   const BYTE aes_sbox[256]);

void aes_encrypt(const BYTE in[],             // 16 bytes of plaintext
                 BYTE out[],                  // 16 bytes of ciphertext
                 const WORD key[],            // From the key setup
                 int keysize,                // Bit length of the key, 128, 192, or 256
                 const BYTE aes_sbox[256], BYTE const aes_invsbox[256], const BYTE gf_mul[1536]);

void aes_decrypt(const BYTE in[],             // 16 bytes of ciphertext
                 BYTE out[],                  // 16 bytes of plaintext
                 const WORD key[],            // From the key setup
                 int keysize,                // Bit length of the key, 128, 192, or 256
                 const BYTE aes_sbox[256], BYTE const aes_invsbox[256], const BYTE gf_mul[1536]);

///////////////////
// AES - CBC
///////////////////
int aes_encrypt_cbc(const BYTE in[],          // Plaintext
                    size_t in_len,            // Must be a multiple of AES_BLOCK_SIZE
                    BYTE out[],               // Ciphertext, same length as plaintext
                    const WORD key[],         // From the key setup
                    int keysize,              // Bit length of the key, 128, 192, or 256
                    const BYTE iv[],         // IV, must be AES_BLOCK_SIZE bytes long
                    const BYTE aes_sbox[256], BYTE const aes_invsbox[256], const BYTE gf_mul[1536]);

int aes_decrypt_cbc(const BYTE in[], 
                        size_t in_len, 
                        BYTE out[], 
                        const WORD key[], 
                        int keysize, 
                        const BYTE iv[],
                        const BYTE aes_sbox[256], BYTE const aes_invsbox[256], const BYTE gf_mul[1536]);

///////////////////
// Test functions
///////////////////
int aes_test();
int aes_cbc_test();

///////////////////
// FaCT functions
///////////////////
void xor_buf(const BYTE in[], BYTE out[], size_t len);
void fmemcpy(BYTE out[], const BYTE in[], size_t len);
WORD modu32(WORD x, WORD y);

#endif   // AES_H
