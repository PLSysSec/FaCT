#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>
#include "stats.h"
#include TARGET_LIB

#define NUM_TRIALS 512
#define NUM_ITRS 512
#define WARMUP_COUNT 3

// Compiling entire file at -O0
//if defined(__clang__)
//define NO_OPT __attribute__((optnone))
//elif defined(__GNUC__)
//define NO_OPT __attribute__((optimize("O0")))
//endif

// Now defined in TARGET_LIB since C preprocessor can't parse multiple arguments :(
//ifndef barrier_data
//define barrier_data(ptr) __asm__ __volatile__("# barrier": :"r"(ptr) :"memory");
//endif

#define TIME_DEC \
    uint64_t cycles_high; \
    uint64_t cycles_low;

#define TIME_START(time) \
    time = 0; \
    asm volatile ("cpuid\n\t" \
		    "rdtsc\n\t" \
		    "mov %%rdx, %0\n\t" \
		    "mov %%rax, %1\n\t" \
		    : "=r" (cycles_high), "=r" (cycles_low) \
		    :: "%rax", "%rbx", "%rcx", "%rdx"); \
    time = (cycles_high << 32) | cycles_low; 

#define TIME_STOP(time) \
    time = 0; \
    asm volatile ("rdtscp\n\t" \
    		    "mov %%rdx, %0\n\t" \
		    "mov %%rax, %1\n\t" \
		    "cpuid\n\t" \
		    : "=r" (cycles_high), "=r" (cycles_low) \
		    :: "%rax", "%rbx", "%rcx", "%rdx"); \
    time = (cycles_high << 32) | cycles_low;

double test(void) {
	uint32_t ctr = 0;
	uint8_t real = 0;
	uint64_t start, stop;
	uint64_t offset_time;
	uint64_t routine_time;
        TIME_DEC
        ROUTINE_DEC

runme:
        ROUTINE_INIT

        TIME_START(start)
	__asm__ __volatile__("# start routine loop");

	for(ctr=0;ctr<NUM_ITRS;ctr++){
		ROUTINE
	        BARRIER_DATA
	}

	__asm__ __volatile__("# end routine loop");
        TIME_STOP(stop)
	routine_time = stop-start;

        TIME_START(start)
	__asm__ __volatile__("# start offset loop");

	for(ctr=0;ctr<NUM_ITRS;ctr++){
		BARRIER_DATA
	}

	__asm__ __volatile__("# end offset loop");
        TIME_STOP(stop)
	offset_time = stop-start;

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

