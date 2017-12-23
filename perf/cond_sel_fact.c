#include <stdint.h>

#include "cond_sel.h"
#include "benchmark.h"

void wrapper() {
  cond_sel(1,1,1);
}

int main() {
  benchmark(wrapper);
}