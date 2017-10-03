`sort.ll` can be fed into ct-verif, and it is produced from `sort.c`, `smack.h`, and `ct-verif.h` via:

`~/fact/Constanc/ct-verif/verifying-constant-time/examples/sort# clang -S -emit-llvm -I ~/fact/Constanc/ct-verif/verifying-constant-time/tools/smack/share/smack/include sort.c`

(produced in the lxc)
