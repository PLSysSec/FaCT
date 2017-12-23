type opt_level = O0 | O1 | O2

val run_optimizations : opt_level -> Llvm.llmodule -> unit