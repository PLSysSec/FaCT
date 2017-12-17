
type mode = DEV | PROD
[@@deriving show]

(* Creates a fresh function environment with all of the standard fact functions
   already defined *)
val make_fenv : unit -> Tast.function_dec Env.env

val codegen_proto : Llvm.llcontext
                 -> Llvm.llmodule
                 -> Tast.fun_name
                 -> Llvm.llvalue
      
val functions : (Tast.fun_name * Tast.function_dec) list