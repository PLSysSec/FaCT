#include <smack.h>
#include "ct-verif.h"

void sort3(int *conds, int *out3, int *in3);

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
