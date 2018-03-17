#include "/root/verifying-constant-time/examples/ct-verif.h"
#include <stdlib.h>

void add5(int num);

void wrapper(int num) {
        add5(num);
}
