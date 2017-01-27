#ifndef SORT_H
#define SORT_H

#define ROUTINE_INIT \
    int conds[3]; \
    int out[3]; \
    int in[3]; 

#define ROUTINE_REINIT \
    reinit(conds,out,in);

void reinit(int *conds,int *out,int *in) {
    memcpy(conds, (int [3]){0,0,0}, 3*sizeof(int));
    memcpy(out, (int [3]){0,0,0}, 3*sizeof(int));
    memcpy(in, (int [3]){6,1,3}, 3*sizeof(int));
}

#define ROUTINE(params) \
    sort3(params);

#define PARAMS \
    conds,out,in

void sort3(int *conds, int *out, int *in);

#endif
