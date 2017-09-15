open Llvm
open Env

type fentry
type fenv
type codegen_ctx_record
type renv

val new_renv : unit -> renv

val mk_ctx : llcontext
          -> llmodule
          -> llbuilder
          -> llvalue env
          -> fenv
          -> Tast.variable_type env
          -> renv
          -> codegen_ctx_record

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

val codegen_stm : codegen_ctx_record
               -> Tast.expr_type' option
               -> Tast.statement
               -> unit

val allocate_stack : codegen_ctx_record
                  -> Tast.block
                  -> unit

val declare_prototype : llcontext
                     -> llmodule
                     -> llbuilder
                     -> fenv
                     -> Tast.params
                     -> Tast.ret_type
                     -> Tast.fun_name
                     -> Llvm.llvalue

val declare_prototypes : llcontext
                      -> llmodule
                      -> llbuilder
                      -> fenv
                      -> Tast.expr' * Tast.expr_type'
                      -> unit