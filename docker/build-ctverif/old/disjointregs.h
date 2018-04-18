#ifndef DISJOINTREGS_H
#define DISJOINTREGS_H

#include <stddef.h>

void disjoint_regs(void *addr1, size_t len1, void *addr2, size_t len2);

#endif
