type cost =
  | Known of int
  | Unknown

(* Each LLVM module has an associated "cost". This is used by the clang
   optimizer to compare IR before and after a transform pass. This function
   exposes that LLVM function to be used by the FaCT optimizer *)
val generate_cost : Llvm.llmodule -> cost