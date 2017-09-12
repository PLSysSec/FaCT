type intrinsic = 
  | Memcpy

val get_intrinsic : intrinsic -> Llvm.llvalue