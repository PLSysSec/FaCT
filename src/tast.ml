open Lexing
open Ast
open Env

type tconstantc_module = TCModule of tfdec list
[@@deriving show, eq]

and tfdec' = { t_name:string; t_params:param list; t_rty:ctype; t_rlbl:label; t_body:tblock }
[@@deriving show, eq]
and tfdec = tfdec' pos_ast [@@deriving show, eq]

and tstm' =
  | TVarDec of string * labeled_type * texpr
  | TAssign of string * texpr
  | TArrAssign of string * texpr * texpr
  | TIf of texpr * tblock * tblock
  | TFor of string * ctype * texpr * texpr * tblock
  | TReturn of texpr
[@@deriving show, eq]
and tstm = tstm' pos_ast [@@deriving show, eq]

and tblock = { venv:Env.env; body:tstm list }
[@@deriving show]

and texpr' = { e:texpr_base; e_ty:ctype; e_lbl:label }
[@@deriving show, eq]
and texpr = texpr' pos_ast [@@deriving show, eq]

and texpr_base =
  | TVarExp of string
  | TArrExp of string * texpr
  | TUnOp of unop * texpr
  | TBinOp of binop * texpr * texpr
  | TPrimitive of tprimitive
  | TCallExp of string * targ list
[@@deriving show, eq]

and targ' =
  | TValArg of texpr
  | TVarArg of string * labeled_type
  | TArrArg of string * labeled_type
[@@deriving show, eq]
and targ = targ' pos_ast [@@deriving show, eq]

and tprimitive' =
  | TNumber of Z.t [@printer Z.pp_print]
  | TBoolean of bool
  | TArrayLiteral of texpr list
[@@deriving show, eq]
and tprimitive = tprimitive' pos_ast [@@deriving show, eq]

let update_fn venv { pos=p; data=tfdec } =
  let args = List.map (fun { data={ lt } } -> lt) tfdec.t_params in
  Hashtbl.add venv tfdec.t_name (Env.FunEntry { f_rty=tfdec.t_rty; f_rlbl=tfdec.t_rlbl; f_args=args })
