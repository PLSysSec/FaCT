/*

This is modified from Manuel's example to remove modularity

For the entry point, parameters must be tagged as to their
security level when operating as inputs and possibly as
outputs.

*/

int sort2(int *out2, int *in2) {
  int a, b;
  a = in2[0];
  b = in2[1];
  if (a < b) {
    out2[0] = in2[0];
    out2[1] = in2[1];
  } else {
    out2[0] = in2[1];
    out2[1] = in2[0];
  }
  return (a < b);
}

void sort3(int *conds, int *out3, int *in3) {
  conds[0] = sort2(out3,in3);
  in3[1] = out3[1];
  conds[1] = sort2(out3+1,in3+1);
  in3[0] = out3[0];
  in3[1] = out3[1];
  conds[2] = sort2(out3,in3);
}


