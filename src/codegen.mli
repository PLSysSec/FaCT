open Llvm
open Env

type fentry
type fenv
type codegen_ctx_record

val new_fenv : unit -> fenv

val codegen : llcontext
           -> llmodule
           -> llbuilder
           -> Tast.fact_module
           -> unit

val codegen_expr : codegen_ctx_record
                -> Tast.expr' * Tast.expr_type'
                -> llvalue

val codegen_fun : llcontext
               -> llmodule
               -> llbuilder
               -> fenv
               -> Tast.function_dec
               -> Llvm.llvalue
