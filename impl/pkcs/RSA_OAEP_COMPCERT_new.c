// #define MAX_CAOINT_SIZE 4096
// #define NBITS 30
// #define MAX_CAOINT_LEN (MAX_CAOINT_SIZE / NBITS + 1) // == 137
// typedef long verylong[MAX_CAOINT_LEN] // eqv int32[137]
// typedef verylong CAO_int
// #define hLen 20
// #define nLen 512
// #define dbLen nLen-hLen-1 // == 491
// #define msgLen dbLen-hLen-1 // == 470
void c_Dec(uint8 _r0[470], int32 _r1[1],
    int32 c_d[137], public int32 c_c[137],
    public uint8 c_lHash[20], public int32 c_n[137]) {
  uint8 c_DB[491] = {0};
  uint8 c_dbMask[dbLen] = {0};
  uint8 c_maskedDB[dbLen] = {0};
  uint8 c_seedMask[hLen] = {0};
  uint8 c_maskedSeed[hLen] = {0};
  uint8 c_lHash2[hLen] = {0};
  uint8 c_seed[hLen] = {0};
  uint8 c_payload[nLen] = {0};
  uint8 c_msg[msgLen] = {0};
  int32 c_result;
  int32 c_result1;
  int32 c_result2;
  int32 c_result3;
  // this is garbage
}

/* Compares two integers.
   Returns 1 (aa > bb), -1 (aa-bb) or 0 (aa=bb) */
int32 zcompare(secret int32 aa[137], secret int32 bb[137]) {
  for (int32 i = 137 to 0 by -1) {
    uint32 bit = (uint32)(bb[i-1] - aa[i-1]); // secret
    bit >>= 31;
  }
}
