open Llvm
open Tast

type fentry = { ret_ty:Tast.ret_type; args:Tast.params }
[@@deriving show]

type fenv = (string,fentry) Hashtbl.t [@printer Env.pp_hashtbl]
[@@deriving show]

type codegen_ctx_record = {
  llcontext   : llcontext;
  llmodule    : llmodule;
  builder     : llbuilder;
  venv        : llvalue Env.env;
  fenv        : fenv;
  tenv        : array_type Env.env;
  vtenv       : variable_type Env.env;
  sdecs       : (string * lltype) list;
  verify_llvm : bool;
}

val bt_to_llvm_ty : llcontext -> base_type' -> lltype

val expr_ty_to_base_ty : expr_type' -> base_type'
