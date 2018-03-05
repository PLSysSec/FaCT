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
  sdecs       : (string * (lltype * struct_type')) list;
  verify_llvm : bool;
}

let bt_to_llvm_ty llctx = function
  | UInt size when size <= 8  -> i8_type llctx
  | UInt size when size <= 16 -> i16_type llctx
  | UInt size when size <= 32 -> i32_type llctx
  | UInt  size when size <= 64 -> i64_type llctx
  | UInt  size when size <= 128 -> integer_type llctx 128
  | Int  size when size <= 8  -> i8_type llctx
  | Int  size when size <= 16 -> i16_type llctx
  | Int  size when size <= 32 -> i32_type llctx
  | Int  size when size <= 64 -> i64_type llctx
  | Bool                      -> i1_type llctx (* TODO: Double check this*)
  | Num(i,s)                  ->
    let rec numbits = function
      | n when n >= -128 && n <= 127 -> 8
      | n -> 8 + (numbits (n / 256))
    in
      integer_type llctx (numbits i)
  | String -> pointer_type (i8_type llctx)

let expr_ty_to_base_ty = function
  | BaseET({data=base_type},_) -> base_type
  | ArrayET({data=ArrayAT(bt,size)},_,_) -> bt.data
