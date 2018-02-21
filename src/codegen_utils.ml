open Tast
open Llvm

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
  verify_llvm : bool;
}

let bt_to_llvm_ty cg_ctx = function
  | UInt size when size <= 8  -> i8_type cg_ctx.llcontext
  | UInt size when size <= 16 -> i16_type cg_ctx.llcontext
  | UInt size when size <= 32 -> i32_type cg_ctx.llcontext
  | UInt  size when size <= 64 -> i64_type cg_ctx.llcontext
  | UInt  size when size <= 128 -> integer_type cg_ctx.llcontext 128
  | Int  size when size <= 8  -> i8_type cg_ctx.llcontext
  | Int  size when size <= 16 -> i16_type cg_ctx.llcontext
  | Int  size when size <= 32 -> i32_type cg_ctx.llcontext
  | Int  size when size <= 64 -> i64_type cg_ctx.llcontext
  | Bool                      -> i1_type cg_ctx.llcontext (* TODO: Double check this*)
  | Num(i,s)                  ->
    let rec numbits = function
      | n when n >= -255 && n <= 256 -> 8
      | n -> 8 + (numbits (n / 256))
    in
      integer_type cg_ctx.llcontext (numbits i)
  | String -> pointer_type (i8_type cg_ctx.llcontext)

let expr_ty_to_base_ty = function
  | BaseET({data=base_type},_) -> base_type
  | ArrayET({data=ArrayAT(bt,size)},_,_) -> bt.data