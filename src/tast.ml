open Lexing
open Ast

type tconstantc_module = TCModule of tfdec list
[@@deriving show, eq]

and tfdec' = { t_name:string; t_params:param list; t_rty:ctype; t_rlbl:label; t_body:tstm list }
[@@deriving show, eq]
and tfdec = tfdec' pos_ast [@@deriving show, eq]

and tstm' =
  | TVarDec of string * labeled_type * texpr
  | TAssign of string * texpr
  | TArrAssign of string * texpr * texpr
  | TIf of texpr * tstm list * tstm list
  | TFor of string * ctype * texpr * texpr * tstm list
  | TReturn of texpr
[@@deriving show, eq]
and tstm = tstm' pos_ast [@@deriving show, eq]

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
