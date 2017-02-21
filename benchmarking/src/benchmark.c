#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>
#include "stats.h"
#include "inteltime.h"
#include TARGET_LIB

#define NUM_TRIALS 512
#define NUM_ITRS 512
#define WARMUP_COUNT 3

double test(void) {
	uint32_t ctr = 0;
	uint8_t real = 0;
	uint64_t offset_time;
	uint64_t routine_time;
        TIME_DEC
        ROUTINE_DEC

runme:
        // routine loop
        TIME_START
	for(ctr=0;ctr<NUM_ITRS;ctr++){
                ROUTINE_INIT
		ROUTINE
	        BARRIER_DATA
	}
        TIME_STOP
        TIME_DIFF(routine_time)

        // offset loop
        TIME_START
	for(ctr=0;ctr<NUM_ITRS;ctr++){
                ROUTINE_INIT
		BARRIER_DATA
	}
        TIME_STOP
        TIME_DIFF(offset_time)

	if(real < WARMUP_COUNT){ real++; goto runme;}

	return (int64_t)(routine_time - offset_time) / (double) NUM_ITRS;
}

void print_times(const double * times, int n) {
	int i;
	
	for(i = 0; i < n; i++)
		printf("%.9f\n", times[i]);
}

int main(int argc, char* argv[]) {
	static double times[NUM_TRIALS];
	double median, mean, stddev;
	int i;

	for(i = 0; i < NUM_TRIALS; i++)
		times[i] = test();

#ifdef RAWOUT
	print_times(times, NUM_TRIALS);
#else
	median = get_median(times, NUM_TRIALS);
	mean = get_mean(times, NUM_TRIALS);
	stddev = get_standard_deviation(times, NUM_TRIALS, mean);

        printf("%-80s %.9f %.9f %.9f \n",
		argv[0], median, mean, stddev);

#endif
}
