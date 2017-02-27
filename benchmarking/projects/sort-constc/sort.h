#ifndef SORT_H
#define SORT_H

#define ROUTINE_DEC \
    int out[3]; \
    int in[3]; 

#define ROUTINE_INIT \
    memcpy(out, (int [3]){0,0,0}, 3*sizeof(int)); \
    memcpy(in, (int [3]){6,1,3}, 3*sizeof(int));

#define ROUTINE sort3(out,in);
int sort3(int *out, int *in);

#define BARRIER_DATA __asm__ __volatile__("# barrier": :"r"(out), "r"(in) :"memory");

#endif
