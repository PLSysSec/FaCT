open Llvm

type fentry
type fenv
type codegen_ctx_record = {
  llcontext   : llcontext;
  llmodule    : llmodule;
  builder     : llbuilder;
  venv        : llvalue Env.env;
  fenv        : fenv;
  tenv        : Tast.array_type Env.env;
  vtenv       : Tast.variable_type Env.env;
  verify_llvm : bool;
}

val mk_ctx : llcontext
          -> llmodule
          -> llbuilder
          -> llvalue Env.env
          -> fenv
          -> Tast.array_type Env.env
          -> Tast.variable_type Env.env
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
               -> bool (* Has the statement returned? *)

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
