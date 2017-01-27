#ifndef SORT_H
#define SORT_H

#define ROUTINE_DEC \
    int conds[3]; \
    int out[3]; \
    int in[3]; 

#define ROUTINE_INIT reinit(conds,out,in);
void reinit(int *conds,int *out,int *in) {
    memcpy(conds, (int [3]){0,0,0}, 3*sizeof(int));
    memcpy(out, (int [3]){0,0,0}, 3*sizeof(int));
    memcpy(in, (int [3]){6,1,3}, 3*sizeof(int));
}

#define ROUTINE sort3(conds,out,in);
void sort3(int *conds, int *out, int *in);

#define BARRIER_DATA __asm__ __volatile__("# barrier": :"r"(conds), "r"(out), "r"(in) :"memory");

#endif
