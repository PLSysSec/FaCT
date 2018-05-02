open Llvm
open Tast

type fentry = { ret_ty:Tast.ret_type; args:Tast.params }
[@@deriving show]

type fenv = (string,fentry) Hashtbl.t [@printer Env.pp_hashtbl]
[@@deriving show]

type codegen_ctx_record = {
  llcontext    : llcontext;
  llmodule     : llmodule;
  builder      : llbuilder;
  venv         : llvalue Env.env;
  fenv         : fenv;
  tenv         : array_type Env.env;
  vtenv        : variable_type Env.env;
  sdecs        : (string * (lltype * struct_type')) list;
  verify_llvm  : bool;
  noinline     : llattribute;
  alwaysinline : llattribute;
}

val mk_ctx : llcontext
          -> llmodule
          -> llbuilder
          -> llvalue Env.env
          -> fenv
          -> Tast.array_type Env.env
          -> Tast.variable_type Env.env
          -> (string * (Llvm.lltype * Tast.struct_type')) list
          -> bool
          -> codegen_ctx_record

val bt_to_llvm_ty : llcontext -> base_type' -> lltype

val expr_ty_to_base_ty : expr_type' -> base_type'
