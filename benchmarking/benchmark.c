#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define NUM_TRIALS 512
#define NUM_ITRS 512
#define WARMUP_COUNT 3

// temporary //
#include <sys/types.h>
#include <unistd.h>
#define ROUTINE getpid()
///////////////


#if defined(__clang__)
#define NO_OPT __attribute__((optnone))
#elif defined(__GNUC__)
#define NO_OPT __attribute__((optimize("O0")))
#endif

#ifndef barrier_data
#define barrier_data(ptr) \
	__asm__ __volatile__("# barrier": :"r"(ptr) :"memory")
#endif

#define TIME_INIT \
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

NO_OPT
double test(void) {
	uint32_t ctr = 0;
	uint8_t real = 0;
	uint64_t start, stop;
	uint64_t offset_time;
	uint64_t routine_time;
        TIME_INIT

runme:
        TIME_START(start)
	__asm__ __volatile__("# start routine loop");

	for(ctr=0;ctr<NUM_ITRS;ctr++){
		ROUTINE;
		//barrier_data(buf);
	}

	__asm__ __volatile__("# end routine loop");
        TIME_STOP(stop)
	routine_time = stop-start;

        TIME_START(start)
	__asm__ __volatile__("# start offset loop");

	for(ctr=0;ctr<NUM_ITRS;ctr++){
		//barrier_data(buf);
	}

	__asm__ __volatile__("# end offset loop");
        TIME_STOP(stop)
	offset_time = stop-start;

	if(real < WARMUP_COUNT){ real++; goto runme;}

	return (int64_t)(routine_time - offset_time) / (double) NUM_ITRS;
}

/*
NO_OPT
void warmup_dcache(void * buf) { 
	int i;
	for(i = 0; i < WARMUP_COUNT; i++) {
		memset(buf, 0x34, NBYTES);
		barrier_data(buf);
	}
}
*/

void print_times(const double * times, int n) {
	int i;
	
	for(i = 0; i < n; i++)
		printf("%.9f\n", times[i]);
}

int compare_double(const void * aptr, const void * bptr) {
	double a = *(const double*)aptr;
	double b = *(const double*)bptr;
	
	if (a < b)
		return -1;
	else if (a > b)
		return 1;
	else
		return 0;
}

double get_median(const double * data, int n) {
	double * datacopy;
	
	datacopy = malloc(n * sizeof(double));
	memcpy(datacopy, data, n * sizeof(double));
	qsort(datacopy, n, sizeof(double), compare_double);
	return datacopy[n/2];
}

double get_mean(const double * data, int n) {
	double sum=0;
	int i;
	for(i = 0; i < n; ++i) sum+=data[i];
	return sum / n;
}

double get_standard_deviation(const double * data, int n, double mean) {
	double sum_deviation = 0;
	int i;
	for(i = 0; i < n; ++i) sum_deviation+=(data[i]-mean)*(data[i]-mean);
	return sqrt(sum_deviation / n);
}

NO_OPT
int main(int argc, char* argv[]) {
	static double times[NUM_TRIALS];
	double median, mean, stddev;
	int i;

        //setup()        

	//warmup_dcache(buf);
	for(i = 0; i < NUM_TRIALS; i++)
		times[i] = test();

#ifdef RAWOUT
	print_times(times, NUM_TRIALS);
#else
	median = get_median(times, NUM_TRIALS);
	mean = get_mean(times, NUM_TRIALS);
	stddev = get_standard_deviation(times, NUM_TRIALS, mean);

	/*
        printf("%-80s %.9f %.9f %.9f %.9f %.9f\n",
		argv[0], median, mean, stddev,
		median / NBYTES, mean / NBYTES);
        */
        printf("%-80s %.9f %.9f %.9f \n",
		argv[0], median, mean, stddev);

#endif
}

