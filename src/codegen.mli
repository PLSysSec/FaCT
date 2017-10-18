open Llvm
open Env

type fentry
type fenv
type renv
type codegen_ctx_record = {
  llcontext   : llcontext;
  llmodule    : llmodule;
  builder     : llbuilder;
  venv        : llvalue env;
  fenv        : fenv;
  tenv        : Tast.array_type env;
  renv        : renv;
  verify_llvm : bool;
}

val new_renv : unit -> renv

val mk_ctx : llcontext
          -> llmodule
          -> llbuilder
          -> llvalue env
          -> fenv
          -> Tast.array_type env
          -> renv
          -> bool
          -> codegen_ctx_record

val new_fenv : unit -> fenv

val codegen : llcontext
           -> llmodule
           -> llbuilder
           -> bool
           -> Tast.fact_module
           -> unit

val codegen_expr : codegen_ctx_record
                -> Tast.expr' * Tast.expr_type'
                -> llvalue

val codegen_fun : llcontext
               -> llmodule
               -> llbuilder
               -> fenv
               -> bool
               -> Tast.function_dec
               -> Llvm.llvalue

val codegen_stm : codegen_ctx_record
               -> Tast.expr_type' option
               -> Tast.statement
               -> unit

val allocate_stack : codegen_ctx_record
                  -> Tast.block
                  -> unit

val declare_prototype : codegen_ctx_record
                     -> llmodule
                     -> llbuilder
                     -> fenv
                     -> Tast.params
                     -> Tast.ret_type
                     -> Tast.fun_name
                     -> Llvm.llvalue

val declare_prototypes : codegen_ctx_record
                      -> llmodule
                      -> llbuilder
                      -> fenv
                      -> Tast.expr' * Tast.expr_type'
                      -> unit