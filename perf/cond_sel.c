#include <stdint.h>

#include "benchmark.h"
#include "cond_sel.h"


void wrapper() {
  cond_sel(1, 1, 1);
}

int main() {
  benchmark(wrapper);
}