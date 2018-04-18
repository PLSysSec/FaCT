#include <smack.h>
#include <stddef.h>

void disjoint_regs(void *addr1, size_t len1, void *addr2, size_t len2) {
        assume(addr1 + len1 * sizeof(*addr1) < addr2 || \
               addr2 + len2 * sizeof(*addr2) < addr1);
}
