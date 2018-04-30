#include <stdio.h>
#include <stdint.h>
#include "micro.h"

#define RDTSCP
#include "inteltime.h"

#define ITERATIONS 100000000
#define MICRO_ITERATIONS 2000000000
#define WARMUP     1000000

void microc(
    const uint8_t *out,
    uint32_t out_len,
    uint32_t inp_len,
    uint8_t *data,
    uint32_t num);

uint8_t _microc(
    uint32_t j,
    uint32_t inp_len,
    uint8_t out);

int main() {
  uint8_t data[64];
  uint8_t out[48];
  uint64_t i;
  uint64_t difftime;
  TIME_DEC

  for (i = 0; i < 48; i++) {
    out[i] = 1234 - i;
  }

  printf("Micro:\n");

  printf("using FaCT features: ");
  fflush(stdout);
  for (i = 0; i < WARMUP; i++) {
    _micro1(3, 24, 83);
  }
  TIME_START
  for (i = 0; i < MICRO_ITERATIONS; i++) {
    _micro1(3, 24, 83);
  }
  TIME_STOP
  TIME_DIFF(difftime);
  printf("%lf ticks/call\n", ((double)difftime) / MICRO_ITERATIONS);

  printf("C-style bitmasks: ");
  fflush(stdout);
  for (i = 0; i < WARMUP; i++) {
    _micro2(3, 24, 83);
  }
  TIME_START
  for (i = 0; i < MICRO_ITERATIONS; i++) {
    _micro2(3, 24, 83);
  }
  TIME_STOP
  TIME_DIFF(difftime);
  printf("%lf ticks/call\n", ((double)difftime) / MICRO_ITERATIONS);

  printf("Actually C: ");
  fflush(stdout);
  for (i = 0; i < WARMUP; i++) {
    _microc(3, 9, 2);
  }
  TIME_START
  for (i = 0; i < MICRO_ITERATIONS; i++) {
    _microc(3, 9, 2);
  }
  TIME_STOP
  TIME_DIFF(difftime);
  printf("%lf ticks/call\n", ((double)difftime) / MICRO_ITERATIONS);

  printf("Mini:\n");

  printf("using FaCT features: ");
  fflush(stdout);
  for (i = 0; i < WARMUP; i++) {
    micro1(out, 48, 16, data, 13);
  }
  TIME_START
  for (i = 0; i < ITERATIONS; i++) {
    micro1(out, 48, 16, data, 13);
  }
  TIME_STOP
  TIME_DIFF(difftime);
  printf("%lf ticks/call\n", ((double)difftime) / ITERATIONS);

  printf("C-style bitmasks: ");
  fflush(stdout);
  for (i = 0; i < WARMUP; i++) {
    micro2(out, 48, 16, data, 13);
  }
  TIME_START
  for (i = 0; i < ITERATIONS; i++) {
    micro2(out, 48, 16, data, 13);
  }
  TIME_STOP
  TIME_DIFF(difftime);
  printf("%lf ticks/call\n", ((double)difftime) / ITERATIONS);

  printf("Actually C: ");
  fflush(stdout);
  for (i = 0; i < WARMUP; i++) {
    microc(out, 48, 16, data, 13);
  }
  TIME_START
  for (i = 0; i < ITERATIONS; i++) {
    microc(out, 48, 16, data, 13);
  }
  TIME_STOP
  TIME_DIFF(difftime);
  printf("%lf ticks/call\n", ((double)difftime) / ITERATIONS);

  return 0;
}
