
type mode = DEV | PROD
[@@deriving show]

val codegen_proto : Llvm.llcontext
                 -> Llvm.llmodule
                 -> Tast.fun_name
                 -> Llvm.llvalue
      
val functions : (Tast.fun_name * Tast.function_dec) list
