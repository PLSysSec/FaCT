#include <math.h>
#include <string.h>
#include <stdlib.h>

static int compare_double(const void * aptr, const void * bptr) {
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


