/*********************************************************************
* Filename:   aes.c
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    This code is the implementation of the AES algorithm and
              the CTR, CBC, and CCM modes of operation it can be used in.
               AES is, specified by the NIST in in publication FIPS PUB 197,
              availible at:
               * http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf .
              The CBC and CTR modes of operation are specified by
              NIST SP 800-38 A, available at:
               * http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf .
              The CCM mode of operation is specified by NIST SP80-38 C, available at:
               * http://csrc.nist.gov/publications/nistpubs/800-38C/SP800-38C_updated-July20_2007.pdf
*********************************************************************/

/*************************** HEADER FILES ***************************/
#include "aes.h"

/****************************** MACROS ******************************/
// The least significant byte of the word is rotated to the end.
#define KE_ROTWORD(x) (((x) << 8) | ((x) >> 24))

/*******************
* AES - CBC
*******************/
int aes_encrypt_cbc(const BYTE in[], size_t in_len, BYTE out[], const WORD key[], int keysize, const BYTE iv[], const BYTE aes_sbox[256], BYTE const aes_invsbox[256], const BYTE gf_mul[1536])
{
	BYTE buf_in[AES_BLOCK_SIZE], buf_out[AES_BLOCK_SIZE], iv_buf[AES_BLOCK_SIZE];
	int blocks, idx;

	//if (in_len % AES_BLOCK_SIZE != 0)
	if (modu32(in_len, AES_BLOCK_SIZE) != 0)
		return -1;

	blocks = in_len / AES_BLOCK_SIZE; // TODO

	fmemcpy(iv_buf, iv, AES_BLOCK_SIZE);

	for (idx = 0; idx < blocks; idx++) {
		fmemcpy(buf_in, &in[idx * AES_BLOCK_SIZE], AES_BLOCK_SIZE);
		xor_buf(iv_buf, buf_in, AES_BLOCK_SIZE);
		aes_encrypt(buf_in, buf_out, key, keysize, aes_sbox,aes_invsbox,gf_mul); // TODO 
		fmemcpy(&out[idx * AES_BLOCK_SIZE], buf_out, AES_BLOCK_SIZE);
		fmemcpy(iv_buf, buf_out, AES_BLOCK_SIZE);
	}

	return 0;
}

int aes_decrypt_cbc(const BYTE in[], size_t in_len, BYTE out[], const WORD key[], int keysize, const BYTE iv[], const BYTE aes_sbox[256], BYTE const aes_invsbox[256], const BYTE gf_mul[1536])
{
	BYTE buf_in[AES_BLOCK_SIZE], buf_out[AES_BLOCK_SIZE], iv_buf[AES_BLOCK_SIZE];
	int blocks, idx;

	if (in_len % AES_BLOCK_SIZE != 0)
		return -1;

	blocks = in_len / AES_BLOCK_SIZE;

	fmemcpy(iv_buf, iv, AES_BLOCK_SIZE);

	for (idx = 0; idx < blocks; idx++) {
		fmemcpy(buf_in, &in[idx * AES_BLOCK_SIZE], AES_BLOCK_SIZE);
		aes_decrypt(buf_in, buf_out, key, keysize, aes_sbox,aes_invsbox,gf_mul);
		xor_buf(iv_buf, buf_out, AES_BLOCK_SIZE);
		fmemcpy(&out[idx * AES_BLOCK_SIZE], buf_out, AES_BLOCK_SIZE);
		fmemcpy(iv_buf, buf_in, AES_BLOCK_SIZE);
	}

	return 0;
}

/*******************
* AES
*******************/
/////////////////
// KEY EXPANSION
/////////////////

// Substitutes a word using the AES S-Box.
WORD SubWord(WORD word, const BYTE aes_sbox[256])
{
	unsigned int result;

	result = (int)aes_sbox[16*((word >> 4) & 0x0000000F) + (word & 0x0000000F)];
	result += (int)aes_sbox[16*((word >> 12) & 0x0000000F) + ((word >> 8) & 0x0000000F)] << 8;
	result += (int)aes_sbox[16*((word >> 20) & 0x0000000F) + ((word >> 16) & 0x0000000F)] << 16;
	result += (int)aes_sbox[16*((word >> 28) & 0x0000000F) + ((word >> 24) & 0x0000000F)] << 24;
	return(result);
}

// Performs the action of generating the keys that will be used in every round of
// encryption. "key" is the user-supplied input key, "w" is the output key schedule,
// "keysize" is the length in bits of "key", must be 128, 192, or 256.
void aes_key_setup(const BYTE key[], WORD w[], int keysize, const BYTE aes_sbox[256])
{
	int Nb=4,Nr,Nk,idx;
	WORD temp,Rcon[]={0x01000000,0x02000000,0x04000000,0x08000000,0x10000000,0x20000000,
	                  0x40000000,0x80000000,0x1b000000,0x36000000,0x6c000000,0xd8000000,
	                  0xab000000,0x4d000000,0x9a000000};

	switch (keysize) {
		case 128: Nr = 10; Nk = 4; break;
		case 192: Nr = 12; Nk = 6; break;
		case 256: Nr = 14; Nk = 8; break;
		default: return;
	}

	for (idx=0; idx < Nk; ++idx) {
		w[idx] = ((key[4 * idx]) << 24) | ((key[4 * idx + 1]) << 16) |
				   ((key[4 * idx + 2]) << 8) | ((key[4 * idx + 3]));
	}

	for (idx = Nk; idx < Nb * (Nr+1); ++idx) {
		temp = w[idx - 1];
		if ((idx % Nk) == 0)
			temp = SubWord(KE_ROTWORD(temp), aes_sbox) ^ Rcon[(idx-1)/Nk];
		else if (Nk > 6 && (idx % Nk) == 4)
			temp = SubWord(temp, aes_sbox);
		w[idx] = w[idx-Nk] ^ temp;
	}
}

/////////////////
// ADD ROUND KEY
/////////////////

// Performs the AddRoundKey step. Each round has its own pre-generated 16-byte key in the
// form of 4 integers (the "w" array). Each integer is XOR'd by one column of the state.
// Also performs the job of InvAddRoundKey(); since the function is a simple XOR process,
// it is its own inverse.
void AddRoundKey(BYTE state[16], const WORD w[])
{
	BYTE subkey[4];

	// memcpy(subkey,&w[idx],4); // Not accurate for big endian machines
	// Subkey 1
	subkey[0] = w[0] >> 24;
	subkey[1] = w[0] >> 16;
	subkey[2] = w[0] >> 8;
	subkey[3] = w[0];
	state[0] ^= subkey[0];
	state[4] ^= subkey[1];
	state[8] ^= subkey[2];
	state[12]^= subkey[3];
	// Subkey 2
	subkey[0] = w[1] >> 24;
	subkey[1] = w[1] >> 16;
	subkey[2] = w[1] >> 8;
	subkey[3] = w[1];
	state[1] ^= subkey[0];
	state[5] ^= subkey[1];
	state[9] ^= subkey[2];
	state[13] ^= subkey[3];
	// Subkey 3
	subkey[0] = w[2] >> 24;
	subkey[1] = w[2] >> 16;
	subkey[2] = w[2] >> 8;
	subkey[3] = w[2];
	state[2] ^= subkey[0];
	state[6] ^= subkey[1];
	state[10] ^= subkey[2];
	state[14] ^= subkey[3];
	// Subkey 4
	subkey[0] = w[3] >> 24;
	subkey[1] = w[3] >> 16;
	subkey[2] = w[3] >> 8;
	subkey[3] = w[3];
	state[3] ^= subkey[0];
	state[7] ^= subkey[1];
	state[11] ^= subkey[2];
	state[15] ^= subkey[3];
}

/////////////////
// (Inv)SubBytes
/////////////////

// Performs the SubBytes step. All bytes in the state are substituted with a
// pre-calculated value from a lookup table.
void SubBytes(BYTE state[16], const BYTE aes_sbox[256])
{
	state[0*4 + 0] = aes_sbox[16*(state[0*4 + 0] >> 4) + (state[0*4 + 0] & 0x0F)];
	state[0*4 + 1] = aes_sbox[16*(state[0*4 + 1] >> 4) + (state[0*4 + 1] & 0x0F)];
	state[0*4 + 2] = aes_sbox[16*(state[0*4 + 2] >> 4) + (state[0*4 + 2] & 0x0F)];
	state[0*4 + 3] = aes_sbox[16*(state[0*4 + 3] >> 4) + (state[0*4 + 3] & 0x0F)];
	state[1*4 + 0] = aes_sbox[16*(state[1*4 + 0] >> 4) + (state[1*4 + 0] & 0x0F)];
	state[1*4 + 1] = aes_sbox[16*(state[1*4 + 1] >> 4) + (state[1*4 + 1] & 0x0F)];
	state[1*4 + 2] = aes_sbox[16*(state[1*4 + 2] >> 4) + (state[1*4 + 2] & 0x0F)];
	state[1*4 + 3] = aes_sbox[16*(state[1*4 + 3] >> 4) + (state[1*4 + 3] & 0x0F)];
	state[2*4 + 0] = aes_sbox[16*(state[2*4 + 0] >> 4) + (state[2*4 + 0] & 0x0F)];
	state[2*4 + 1] = aes_sbox[16*(state[2*4 + 1] >> 4) + (state[2*4 + 1] & 0x0F)];
	state[2*4 + 2] = aes_sbox[16*(state[2*4 + 2] >> 4) + (state[2*4 + 2] & 0x0F)];
	state[2*4 + 3] = aes_sbox[16*(state[2*4 + 3] >> 4) + (state[2*4 + 3] & 0x0F)];
	state[3*4 + 0] = aes_sbox[16*(state[3*4 + 0] >> 4) + (state[3*4 + 0] & 0x0F)];
	state[3*4 + 1] = aes_sbox[16*(state[3*4 + 1] >> 4) + (state[3*4 + 1] & 0x0F)];
	state[3*4 + 2] = aes_sbox[16*(state[3*4 + 2] >> 4) + (state[3*4 + 2] & 0x0F)];
	state[3*4 + 3] = aes_sbox[16*(state[3*4 + 3] >> 4) + (state[3*4 + 3] & 0x0F)];
}

void InvSubBytes(BYTE state[16], const BYTE aes_invsbox[256])
{
	state[0*4 + 0] = aes_invsbox[16*(state[0*4 + 0] >> 4) + (state[0*4 + 0] & 0x0F)];
	state[0*4 + 1] = aes_invsbox[16*(state[0*4 + 1] >> 4) + (state[0*4 + 1] & 0x0F)];
	state[0*4 + 2] = aes_invsbox[16*(state[0*4 + 2] >> 4) + (state[0*4 + 2] & 0x0F)];
	state[0*4 + 3] = aes_invsbox[16*(state[0*4 + 3] >> 4) + (state[0*4 + 3] & 0x0F)];
	state[1*4 + 0] = aes_invsbox[16*(state[1*4 + 0] >> 4) + (state[1*4 + 0] & 0x0F)];
	state[1*4 + 1] = aes_invsbox[16*(state[1*4 + 1] >> 4) + (state[1*4 + 1] & 0x0F)];
	state[1*4 + 2] = aes_invsbox[16*(state[1*4 + 2] >> 4) + (state[1*4 + 2] & 0x0F)];
	state[1*4 + 3] = aes_invsbox[16*(state[1*4 + 3] >> 4) + (state[1*4 + 3] & 0x0F)];
	state[2*4 + 0] = aes_invsbox[16*(state[2*4 + 0] >> 4) + (state[2*4 + 0] & 0x0F)];
	state[2*4 + 1] = aes_invsbox[16*(state[2*4 + 1] >> 4) + (state[2*4 + 1] & 0x0F)];
	state[2*4 + 2] = aes_invsbox[16*(state[2*4 + 2] >> 4) + (state[2*4 + 2] & 0x0F)];
	state[2*4 + 3] = aes_invsbox[16*(state[2*4 + 3] >> 4) + (state[2*4 + 3] & 0x0F)];
	state[3*4 + 0] = aes_invsbox[16*(state[3*4 + 0] >> 4) + (state[3*4 + 0] & 0x0F)];
	state[3*4 + 1] = aes_invsbox[16*(state[3*4 + 1] >> 4) + (state[3*4 + 1] & 0x0F)];
	state[3*4 + 2] = aes_invsbox[16*(state[3*4 + 2] >> 4) + (state[3*4 + 2] & 0x0F)];
	state[3*4 + 3] = aes_invsbox[16*(state[3*4 + 3] >> 4) + (state[3*4 + 3] & 0x0F)];
}

/////////////////
// (Inv)ShiftRows
/////////////////

// Performs the ShiftRows step. All rows are shifted cylindrically to the left.
void ShiftRows(BYTE state[16])
{
	int t;

	// Shift left by 1
	t = state[1*4 + 0];
	state[1*4 + 0] = state[1*4 + 1];
	state[1*4 + 1] = state[1*4 + 2];
	state[1*4 + 2] = state[1*4 + 3];
	state[1*4 + 3] = t;
	// Shift left by 2
	t = state[2*4 + 0];
	state[2*4 + 0] = state[2*4 + 2];
	state[2*4 + 2] = t;
	t = state[2*4 + 1];
	state[2*4 + 1] = state[2*4 + 3];
	state[2*4 + 3] = t;
	// Shift left by 3
	t = state[3*4 + 0];
	state[3*4 + 0] = state[3*4 + 3];
	state[3*4 + 3] = state[3*4 + 2];
	state[3*4 + 2] = state[3*4 + 1];
	state[3*4 + 1] = t;
}

// All rows are shifted cylindrically to the right.
void InvShiftRows(BYTE state[16])
{
	int t;

	// Shift right by 1
	t = state[1*4 + 3];
	state[1*4 + 3] = state[1*4 + 2];
	state[1*4 + 2] = state[1*4 + 1];
	state[1*4 + 1] = state[1*4 + 0];
	state[1*4 + 0] = t;
	// Shift right by 2
	t = state[2*4 + 3];
	state[2*4 + 3] = state[2*4 + 1];
	state[2*4 + 1] = t;
	t = state[2*4 + 2];
	state[2*4 + 2] = state[2*4 + 0];
	state[2*4 + 0] = t;
	// Shift right by 3
	t = state[3*4 + 3];
	state[3*4 + 3] = state[3*4 + 0];
	state[3*4 + 0] = state[3*4 + 1];
	state[3*4 + 1] = state[3*4 + 2];
	state[3*4 + 2] = t;
}

/////////////////
// (Inv)MixColumns
/////////////////

// Performs the MixColums step. The state is multiplied by itself using matrix
// multiplication in a Galios Field 2^8. All multiplication is pre-computed in a table.
// Addition is equivilent to XOR. (Must always make a copy of the column as the original
// values will be destoyed.)
void MixColumns(BYTE state[16], const BYTE gf_mul[1536])
{
	BYTE col[4];

	// Column 1
	col[0] = state[0*4 + 0];
	col[1] = state[1*4 + 0];
	col[2] = state[2*4 + 0];
	col[3] = state[3*4 + 0];
	state[0*4 + 0] = gf_mul[6*(col[0]) + 0];
	state[0*4 + 0] ^= gf_mul[6*(col[1]) + 1];
	state[0*4 + 0] ^= col[2];
	state[0*4 + 0] ^= col[3];
	state[1*4 + 0] = col[0];
	state[1*4 + 0] ^= gf_mul[6*(col[1]) + 0];
	state[1*4 + 0] ^= gf_mul[6*(col[2]) + 1];
	state[1*4 + 0] ^= col[3];
	state[2*4 + 0] = col[0];
	state[2*4 + 0] ^= col[1];
	state[2*4 + 0] ^= gf_mul[6*(col[2]) + 0];
	state[2*4 + 0] ^= gf_mul[6*(col[3]) + 1];
	state[3*4 + 0] = gf_mul[6*(col[0]) + 1];
	state[3*4 + 0] ^= col[1];
	state[3*4 + 0] ^= col[2];
	state[3*4 + 0] ^= gf_mul[6*(col[3]) + 0];
	// Column 2
	col[0] = state[0*4 + 1];
	col[1] = state[1*4 + 1];
	col[2] = state[2*4 + 1];
	col[3] = state[3*4 + 1];
	state[0*4 + 1] = gf_mul[6*(col[0]) + 0];
	state[0*4 + 1] ^= gf_mul[6*(col[1]) + 1];
	state[0*4 + 1] ^= col[2];
	state[0*4 + 1] ^= col[3];
	state[1*4 + 1] = col[0];
	state[1*4 + 1] ^= gf_mul[6*(col[1]) + 0];
	state[1*4 + 1] ^= gf_mul[6*(col[2]) + 1];
	state[1*4 + 1] ^= col[3];
	state[2*4 + 1] = col[0];
	state[2*4 + 1] ^= col[1];
	state[2*4 + 1] ^= gf_mul[6*(col[2]) + 0];
	state[2*4 + 1] ^= gf_mul[6*(col[3]) + 1];
	state[3*4 + 1] = gf_mul[6*(col[0]) + 1];
	state[3*4 + 1] ^= col[1];
	state[3*4 + 1] ^= col[2];
	state[3*4 + 1] ^= gf_mul[6*(col[3]) + 0];
	// Column 3
	col[0] = state[0*4 + 2];
	col[1] = state[1*4 + 2];
	col[2] = state[2*4 + 2];
	col[3] = state[3*4 + 2];
	state[0*4 + 2] = gf_mul[6*(col[0]) + 0];
	state[0*4 + 2] ^= gf_mul[6*(col[1]) + 1];
	state[0*4 + 2] ^= col[2];
	state[0*4 + 2] ^= col[3];
	state[1*4 + 2] = col[0];
	state[1*4 + 2] ^= gf_mul[6*(col[1]) + 0];
	state[1*4 + 2] ^= gf_mul[6*(col[2]) + 1];
	state[1*4 + 2] ^= col[3];
	state[2*4 + 2] = col[0];
	state[2*4 + 2] ^= col[1];
	state[2*4 + 2] ^= gf_mul[6*(col[2]) + 0];
	state[2*4 + 2] ^= gf_mul[6*(col[3]) + 1];
	state[3*4 + 2] = gf_mul[6*(col[0]) + 1];
	state[3*4 + 2] ^= col[1];
	state[3*4 + 2] ^= col[2];
	state[3*4 + 2] ^= gf_mul[6*(col[3]) + 0];
	// Column 4
	col[0] = state[0*4 + 3];
	col[1] = state[1*4 + 3];
	col[2] = state[2*4 + 3];
	col[3] = state[3*4 + 3];
	state[0*4 + 3] = gf_mul[6*(col[0]) + 0];
	state[0*4 + 3] ^= gf_mul[6*(col[1]) + 1];
	state[0*4 + 3] ^= col[2];
	state[0*4 + 3] ^= col[3];
	state[1*4 + 3] = col[0];
	state[1*4 + 3] ^= gf_mul[6*(col[1]) + 0];
	state[1*4 + 3] ^= gf_mul[6*(col[2]) + 1];
	state[1*4 + 3] ^= col[3];
	state[2*4 + 3] = col[0];
	state[2*4 + 3] ^= col[1];
	state[2*4 + 3] ^= gf_mul[6*(col[2]) + 0];
	state[2*4 + 3] ^= gf_mul[6*(col[3]) + 1];
	state[3*4 + 3] = gf_mul[6*(col[0]) + 1];
	state[3*4 + 3] ^= col[1];
	state[3*4 + 3] ^= col[2];
	state[3*4 + 3] ^= gf_mul[6*(col[3]) + 0];
}

void InvMixColumns(BYTE state[16], const BYTE gf_mul[1536])
{
	BYTE col[4];

	// Column 1
	col[0] = state[0*4 + 0];
	col[1] = state[1*4 + 0];
	col[2] = state[2*4 + 0];
	col[3] = state[3*4 + 0];
	state[0*4 + 0] = gf_mul[6*(col[0]) + 5];
	state[0*4 + 0] ^= gf_mul[6*(col[1]) + 3];
	state[0*4 + 0] ^= gf_mul[6*(col[2]) + 4];
	state[0*4 + 0] ^= gf_mul[6*(col[3]) + 2];
	state[1*4 + 0] = gf_mul[6*(col[0]) + 2];
	state[1*4 + 0] ^= gf_mul[6*(col[1]) + 5];
	state[1*4 + 0] ^= gf_mul[6*(col[2]) + 3];
	state[1*4 + 0] ^= gf_mul[6*(col[3]) + 4];
	state[2*4 + 0] = gf_mul[6*(col[0]) + 4];
	state[2*4 + 0] ^= gf_mul[6*(col[1]) + 2];
	state[2*4 + 0] ^= gf_mul[6*(col[2]) + 5];
	state[2*4 + 0] ^= gf_mul[6*(col[3]) + 3];
	state[3*4 + 0] = gf_mul[6*(col[0]) + 3];
	state[3*4 + 0] ^= gf_mul[6*(col[1]) + 4];
	state[3*4 + 0] ^= gf_mul[6*(col[2]) + 2];
	state[3*4 + 0] ^= gf_mul[6*(col[3]) + 5];
	// Column 2
	col[0] = state[0*4 + 1];
	col[1] = state[1*4 + 1];
	col[2] = state[2*4 + 1];
	col[3] = state[3*4 + 1];
	state[0*4 + 1] = gf_mul[6*(col[0]) + 5];
	state[0*4 + 1] ^= gf_mul[6*(col[1]) + 3];
	state[0*4 + 1] ^= gf_mul[6*(col[2]) + 4];
	state[0*4 + 1] ^= gf_mul[6*(col[3]) + 2];
	state[1*4 + 1] = gf_mul[6*(col[0]) + 2];
	state[1*4 + 1] ^= gf_mul[6*(col[1]) + 5];
	state[1*4 + 1] ^= gf_mul[6*(col[2]) + 3];
	state[1*4 + 1] ^= gf_mul[6*(col[3]) + 4];
	state[2*4 + 1] = gf_mul[6*(col[0]) + 4];
	state[2*4 + 1] ^= gf_mul[6*(col[1]) + 2];
	state[2*4 + 1] ^= gf_mul[6*(col[2]) + 5];
	state[2*4 + 1] ^= gf_mul[6*(col[3]) + 3];
	state[3*4 + 1] = gf_mul[6*(col[0]) + 3];
	state[3*4 + 1] ^= gf_mul[6*(col[1]) + 4];
	state[3*4 + 1] ^= gf_mul[6*(col[2]) + 2];
	state[3*4 + 1] ^= gf_mul[6*(col[3]) + 5];
	// Column 3
	col[0] = state[0*4 + 2];
	col[1] = state[1*4 + 2];
	col[2] = state[2*4 + 2];
	col[3] = state[3*4 + 2];
	state[0*4 + 2] = gf_mul[6*(col[0]) + 5];
	state[0*4 + 2] ^= gf_mul[6*(col[1]) + 3];
	state[0*4 + 2] ^= gf_mul[6*(col[2]) + 4];
	state[0*4 + 2] ^= gf_mul[6*(col[3]) + 2];
	state[1*4 + 2] = gf_mul[6*(col[0]) + 2];
	state[1*4 + 2] ^= gf_mul[6*(col[1]) + 5];
	state[1*4 + 2] ^= gf_mul[6*(col[2]) + 3];
	state[1*4 + 2] ^= gf_mul[6*(col[3]) + 4];
	state[2*4 + 2] = gf_mul[6*(col[0]) + 4];
	state[2*4 + 2] ^= gf_mul[6*(col[1]) + 2];
	state[2*4 + 2] ^= gf_mul[6*(col[2]) + 5];
	state[2*4 + 2] ^= gf_mul[6*(col[3]) + 3];
	state[3*4 + 2] = gf_mul[6*(col[0]) + 3];
	state[3*4 + 2] ^= gf_mul[6*(col[1]) + 4];
	state[3*4 + 2] ^= gf_mul[6*(col[2]) + 2];
	state[3*4 + 2] ^= gf_mul[6*(col[3]) + 5];
	// Column 4
	col[0] = state[0*4 + 3];
	col[1] = state[1*4 + 3];
	col[2] = state[2*4 + 3];
	col[3] = state[3*4 + 3];
	state[0*4 + 3] = gf_mul[6*(col[0]) + 5];
	state[0*4 + 3] ^= gf_mul[6*(col[1]) + 3];
	state[0*4 + 3] ^= gf_mul[6*(col[2]) + 4];
	state[0*4 + 3] ^= gf_mul[6*(col[3]) + 2];
	state[1*4 + 3] = gf_mul[6*(col[0]) + 2];
	state[1*4 + 3] ^= gf_mul[6*(col[1]) + 5];
	state[1*4 + 3] ^= gf_mul[6*(col[2]) + 3];
	state[1*4 + 3] ^= gf_mul[6*(col[3]) + 4];
	state[2*4 + 3] = gf_mul[6*(col[0]) + 4];
	state[2*4 + 3] ^= gf_mul[6*(col[1]) + 2];
	state[2*4 + 3] ^= gf_mul[6*(col[2]) + 5];
	state[2*4 + 3] ^= gf_mul[6*(col[3]) + 3];
	state[3*4 + 3] = gf_mul[6*(col[0]) + 3];
	state[3*4 + 3] ^= gf_mul[6*(col[1]) + 4];
	state[3*4 + 3] ^= gf_mul[6*(col[2]) + 2];
	state[3*4 + 3] ^= gf_mul[6*(col[3]) + 5];
}

/////////////////
// (En/De)Crypt
/////////////////

void aes_encrypt(const BYTE in[], BYTE out[], const WORD key[], int keysize, const BYTE aes_sbox[256], const BYTE aes_invsbox[256], const BYTE gf_mul[1536])
{
	BYTE state[16];

	// Copy input array (should be 16 bytes long) to a matrix (sequential bytes are ordered
	// by row, not col) called "state" for processing.
	// *** Implementation note: The official AES documentation references the state by
	// column, then row. Accessing an element in C requires row then column. Thus, all state
	// references in AES must have the column and row indexes reversed for C implementation.
	state[0*4 + 0] = in[0];
	state[1*4 + 0] = in[1];
	state[2*4 + 0] = in[2];
	state[3*4 + 0] = in[3];
	state[0*4 + 1] = in[4];
	state[1*4 + 1] = in[5];
	state[2*4 + 1] = in[6];
	state[3*4 + 1] = in[7];
	state[0*4 + 2] = in[8];
	state[1*4 + 2] = in[9];
	state[2*4 + 2] = in[10];
	state[3*4 + 2] = in[11];
	state[0*4 + 3] = in[12];
	state[1*4 + 3] = in[13];
	state[2*4 + 3] = in[14];
	state[3*4 + 3] = in[15];

	// Perform the necessary number of rounds. The round key is added first.
	// The last round does not perform the MixColumns step.
	AddRoundKey(state,&key[0]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[4]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[8]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[12]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[16]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[20]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[24]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[28]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[32]);
	SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[36]);
	if (keysize != 128) {
		SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[40]);
		SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[44]);
		if (keysize != 192) {
			SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[48]);
			SubBytes(state, aes_sbox); ShiftRows(state); MixColumns(state, gf_mul); AddRoundKey(state,&key[52]);
			SubBytes(state, aes_sbox); ShiftRows(state); AddRoundKey(state,&key[56]);
		}
		else {
			SubBytes(state, aes_sbox); ShiftRows(state); AddRoundKey(state,&key[48]);
		}
	}
	else {
		SubBytes(state, aes_sbox); ShiftRows(state); AddRoundKey(state,&key[40]);
	}

	// Copy the state to the output array.
	out[0] = state[0*4 + 0];
	out[1] = state[1*4 + 0];
	out[2] = state[2*4 + 0];
	out[3] = state[3*4 + 0];
	out[4] = state[0*4 + 1];
	out[5] = state[1*4 + 1];
	out[6] = state[2*4 + 1];
	out[7] = state[3*4 + 1];
	out[8] = state[0*4 + 2];
	out[9] = state[1*4 + 2];
	out[10] = state[2*4 + 2];
	out[11] = state[3*4 + 2];
	out[12] = state[0*4 + 3];
	out[13] = state[1*4 + 3];
	out[14] = state[2*4 + 3];
	out[15] = state[3*4 + 3];
}

void aes_decrypt(const BYTE in[], BYTE out[], const WORD key[], int keysize, const BYTE aes_sbox[256], BYTE const aes_invsbox[256], const BYTE gf_mul[1536])
{
	BYTE state[16];

	// Copy the input to the state.
	state[0*4 + 0] = in[0];
	state[1*4 + 0] = in[1];
	state[2*4 + 0] = in[2];
	state[3*4 + 0] = in[3];
	state[0*4 + 1] = in[4];
	state[1*4 + 1] = in[5];
	state[2*4 + 1] = in[6];
	state[3*4 + 1] = in[7];
	state[0*4 + 2] = in[8];
	state[1*4 + 2] = in[9];
	state[2*4 + 2] = in[10];
	state[3*4 + 2] = in[11];
	state[0*4 + 3] = in[12];
	state[1*4 + 3] = in[13];
	state[2*4 + 3] = in[14];
	state[3*4 + 3] = in[15];

	// Perform the necessary number of rounds. The round key is added first.
	// The last round does not perform the MixColumns step.
	if (keysize > 128) {
		if (keysize > 192) {
			AddRoundKey(state,&key[56]);
			InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[52]);InvMixColumns(state, gf_mul);
			InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[48]);InvMixColumns(state, gf_mul);
		}
		else {
			AddRoundKey(state,&key[48]);
		}
		InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[44]);InvMixColumns(state, gf_mul);
		InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[40]);InvMixColumns(state, gf_mul);
	}
	else {
		AddRoundKey(state,&key[40]);
	}
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[36]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[32]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[28]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[24]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[20]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[16]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[12]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[8]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[4]);InvMixColumns(state, gf_mul);
	InvShiftRows(state);InvSubBytes(state, aes_invsbox);AddRoundKey(state,&key[0]);

	// Copy the state to the output array.
	out[0] = state[0*4 + 0];
	out[1] = state[1*4 + 0];
	out[2] = state[2*4 + 0];
	out[3] = state[3*4 + 0];
	out[4] = state[0*4 + 1];
	out[5] = state[1*4 + 1];
	out[6] = state[2*4 + 1];
	out[7] = state[3*4 + 1];
	out[8] = state[0*4 + 2];
	out[9] = state[1*4 + 2];
	out[10] = state[2*4 + 2];
	out[11] = state[3*4 + 2];
	out[12] = state[0*4 + 3];
	out[13] = state[1*4 + 3];
	out[14] = state[2*4 + 3];
	out[15] = state[3*4 + 3];
}

/*******************
** AES DEBUGGING FUNCTIONS
*******************/
/*
// This prints the "state" grid as a linear hex string.
void print_state(BYTE state[][4])
{
	int idx,idx2;

	for (idx=0; idx < 4; idx++)
		for (idx2=0; idx2 < 4; idx2++)
			printf("%02x",state[idx2][idx]);
	printf("\n");
}

// This prints the key (4 consecutive ints) used for a given round as a linear hex string.
void print_rnd_key(WORD key[])
{
	int idx;

	for (idx=0; idx < 4; idx++)
		printf("%08x",key[idx]);
	printf("\n");
}
*/
