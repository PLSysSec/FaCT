open Pos
open Err
open Ast

type texpr_base =
  | TBoolean of bool
  | TNumber of int
  | TVar of symbol
  | TArrAccess of symbol * texpr
  | TArrComprehension of base_type * size * base_type * symbol * texpr
  | TArrView of symbol * texpr * size
  | TUnOp of unop * texpr
  | TBinOp of binop * texpr * texpr
  | TTernaryOp of texpr * texpr * texpr
  | TRef of symbol
  | TFunCall of symbol * texprs
[@@deriving show, eq]

and texpr' = { expr:texpr_base; description:description }
[@@deriving show]
and texpr = texpr' pos_ast [@@deriving show]

and texprs = texpr list
[@@deriving show, eq]

and tstms = tstm list
[@@deriving show]

and tstm_base =
  | TVarDec of symbol * description * texpr
  | TVarAssign of symbol * texpr
  | TArrAssign of symbol * texpr * texpr
  | TIf of texpr * tstms * tstms
  | TFor of base_type * symbol * texpr * texpr * tstms
  | TReturn of texpr
[@@deriving show, eq]

and tstm = tstm_base pos_ast
[@@deriving show]

type tblock = {
  venv:Ast.description Env.env; [@printer Env.pp_env]
  body:tstms
} [@@deriving show]

type tfdec_base = {
  name:symbol;
  params:params;
  ret_description:ret_description;
  tstms: tstms
} [@@deriving show, eq]

and tfdec = tfdec_base pos_ast
[@@deriving show]

and tfdecs = tfdec list
[@@deriving show, eq]

type fact_module = TCModule of Env.fenv * tfdecs
[@@deriving show]

let update_fn fenv { pos=p; data=tfdec } =
  let args = List.map (fun { data=(s,des) } -> des) tfdec.params in
    if Env.has_fn fenv tfdec.name
      then raise_error p (RedefiningFunction tfdec.name);
    Env.add_fn fenv tfdec.name
      { Env.f_rvt=tfdec.ret_description; Env.f_args=args }
