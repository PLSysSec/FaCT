#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define RDTSCP 1
#include "inteltime.h"

#include <unistd.h>
#include <string.h>

#include "perf.h"

#define NUM_TRIALS 2048
#define NUM_ITRS 2048
#define WARMUP_COUNT 7

double test(void) {
	uint32_t ctr = 0;
	uint8_t real = 0;
	uint64_t offset_time;
	uint64_t routine_time;
        TIME_DEC

runme:
        TIME_START
	__asm__ __volatile__("# start routine loop");

#ifndef TIME_READ_OVERHEAD
	for(ctr=0;ctr<NUM_ITRS;ctr++){
		int32_t x = NUM_ITRS;
		int32_t y = NUM_ITRS / 2;
		int32_t z = cond_sel(x < y, x, y);
	}
#endif

	__asm__ __volatile__("# end routine loop");
        TIME_STOP
	TIME_DIFF(routine_time)

        TIME_START
	__asm__ __volatile__("# start offset loop");

	for(ctr=0;ctr<NUM_ITRS;ctr++){
		int32_t x = NUM_ITRS;
		int32_t y = NUM_ITRS / 2;
		bool z = x < y;
	}

	__asm__ __volatile__("# end offset loop");
        TIME_STOP
	TIME_DIFF(offset_time)

	if(real < WARMUP_COUNT){ real++; goto runme;}

#ifdef SINGLE_LOOP_TIME
        return (int64_t)(routine_time) / (double) NUM_ITRS;
#else
	return (int64_t)(routine_time - offset_time) / (double) NUM_ITRS;
#endif
}

void print_times(const double * times, int n) {
	int i;
	
	for(i = 0; i < n; i++)
		printf("%.9f\n", times[i]);
}

int main(int argc, char* argv[]) {
	static double times[NUM_TRIALS];
	double mean = 0;
	int i;

	for(i = 0; i < NUM_TRIALS; i++)
		times[i] = test();

	for (i = 0; i < NUM_TRIALS; i++) {
		mean += times[i];
	}
	mean /= NUM_TRIALS;
	printf("mean: %.9f\n", mean);

	return 0;
}

