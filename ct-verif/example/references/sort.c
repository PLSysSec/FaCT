/*

This is modified from Manuel's example to remove modularity

For the entry point, parameters must be tagged as to their
security level when operating as inputs and possibly as
outputs.

*/
#include <smack.h>

#include "../ct-verif.h"

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

int* sort3_wrapper(int *conds, int *out, int *in) {
  __disjoint_regions(conds,3,out,3);
  __disjoint_regions(conds,3,in,3);
  __disjoint_regions(out,3,in,3);

  /* Boilerplate */
  public_in(__SMACK_value(conds));
  public_in(__SMACK_value(out));
  public_in(__SMACK_value(in));

  /* Useful */
  declassified_out(__SMACK_values(conds,3));

  /* Testing out more of the assertion generation */
  public_in(__SMACK_values(conds,3));
  public_out(__SMACK_values(conds,3));
  public_out(__SMACK_return_value());

  // This is broken for now, but we should ignore it until we see an
  // example that works like this...
  //declassified_out(__SMACK_return_values(__SMACK_return_value(),3));

  sort3(conds,out,in);
  return conds;
}
