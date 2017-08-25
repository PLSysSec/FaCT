open Llvm
open Env

type fentry
type fenv

val codegen : llcontext
           -> llmodule
           -> llbuilder
           -> Tast.fact_module
           -> unit

val codegen_expr : llcontext
                -> llmodule
                -> llbuilder
                -> llvalue env
                -> fenv
                -> Tast.variable_type env
                -> Tast.expr' * Tast.expr_type'
                -> llvalue

val codegen_fun : llcontext
               -> llmodule
               -> llbuilder
               -> fenv
               -> Tast.function_dec
               -> Llvm.llvalue
