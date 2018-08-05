#include "ctverif.h"
#include "foo.h"

void foo_wrapper(int x) {
        public_in(__SMACK_value(x));
        foo(x);
}
