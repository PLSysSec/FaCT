#include <stdio.h>
#include <stdint.h>
#include "micro.h"

#define RDTSCP
#include "inteltime.h"

#define ITERATIONS 100000000
#define WARMUP     1000000

int main() {
  uint8_t data[64];
  uint8_t out[48];
  uint64_t i;
  uint64_t difftime;
  TIME_DEC

  for (i = 0; i < 48; i++) {
    out[i] = 1234 - i;
  }

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

  return 0;
}
