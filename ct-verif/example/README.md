`sort.ll`can be fed directly into ct-verif. It was produced from `sort.c`, `smack.h`, and `ct-verif.h` in the linux container (from the `references` dir) via:

`~/fact/Constanc/ct-verif/verifying-constant-time/examples/sort# clang -S -emit-llvm -I ~/fact/Constanc/ct-verif/verifying-constant-time/tools/smack/share/smack/include sort.c`

Run `make` and ct-verif will verify `sort.ll`.

