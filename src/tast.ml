open Pos
open Err
open Ast
open Env

type tconstantc_module = TCModule of Env.fenv * tfdec list
[@@deriving show]

and tfdec' = { t_name:string; t_params:param list; t_rvt:var_type; t_body:tblock }
[@@deriving show ]
and tfdec = tfdec' pos_ast [@@deriving show ]

and tstm' =
  | TVarDec of string * var_type * texpr
  | TArrDec of string * var_type * int * arrinit
  | TAssign of string * texpr
  | TArrAssign of string * texpr * texpr
  | TIf of texpr * tblock * tblock
  | TFor of string * ctype * texpr * texpr * tblock
  | TReturn of texpr
[@@deriving show]
and tstm = tstm' pos_ast [@@deriving show]

and tblock = { venv:Ast.labeled_type Env.env; [@printer Env.pp_env]
               body:tstm list }
[@@deriving show]

and texpr' = { e:texpr_base; e_ty:ctype; e_lbl:label }
[@@deriving show]
and texpr = texpr' pos_ast [@@deriving show]

and texpr_base =
  | TVarExp of string
  | TArrExp of string * texpr
  | TUnOp of unop * texpr
  | TBinOp of binop * texpr * texpr
  | TPrimitive of tprimitive
  | TCallExp of string * targ list
[@@deriving show]

and targ' =
  | TValArg of texpr
  | TRefArg of string * var_type
  | TArrArg of string * var_type * int
[@@deriving show]
and targ = targ' pos_ast [@@deriving show]

and tprimitive' =
  | TNumber of int
  | TBoolean of bool
[@@deriving show]
and tprimitive = tprimitive' pos_ast [@@deriving show]

let update_fn fenv { pos=p; data=tfdec } =
  let args = List.map (fun { data={ lt } } -> lt) tfdec.t_params in
    if has_fn fenv tfdec.t_name then raise (UnclassifiedError "redefining fn");
    add_fn fenv tfdec.t_name { f_rvt=tfdec.t_rvt; f_args=args }
