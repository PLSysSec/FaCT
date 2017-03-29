open Pos


type symbol = string
[@@deriving show, eq]

type size = int
[@@deriving show, eq]

type base_type =
  | Bool
  | Int of int
  | UInt of int
[@@deriving show, eq]

type ctype =
  | Base of base_type
  | Arr of base_type * int
[@@deriving show, eq]

type label =
  | Public
  | Secret
  | Unknown
[@@deriving show, eq]

type mutability =
  | Const
  | Mut
[@@deriving show, eq]

type description = ctype * label * mutability
[@@deriving show, eq]

type ret_description = description
[@@deriving show, eq]

type param_base = symbol * description
[@@deriving show, eq]

type param = param_base pos_ast
[@@deriving show, eq]

type params = param list
[@@deriving show, eq]

type unop_base =
  | Negation
  | LogicalNot
  | BitwiseNot
[@@deriving show, eq]

type unop = unop_base pos_ast
[@@deriving show, eq]

type binop_base =
  | Plus
  | Minus
  | Multiply
  | Equal
  | NEqual
  | GT
  | GTE
  | LT
  | LTE
  | LogicalAnd
  | LogicalOr
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LeftShift
  | RightShift
[@@deriving show, eq]

type binop = binop_base pos_ast
[@@deriving show, eq]

type expr_base =
  | Boolean of bool
  | Number of int
  | Var of symbol
  | ArrAccess of symbol * expr
  | ArrComprehension of base_type * size * base_type * symbol * expr
  | ArrView of symbol * expr * size
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | TernaryOp of expr * expr * expr
  | Ref of symbol
  | FunCall of symbol * exprs
[@@deriving show, eq]

and expr = expr_base pos_ast
[@@deriving show, eq]

and exprs = expr list
[@@deriving show, eq]

and stms = stm list
[@@deriving show, eq]

and stm_base =
  | VarDec of symbol * description * expr
  | VarAssign of symbol * expr
  | ArrAssign of symbol * expr * expr
  | If of expr * stms * stms
  | For of base_type * symbol * expr * expr * stms
  | Return of expr
[@@deriving show, eq]

and stm = stm_base pos_ast
[@@deriving show, eq]

type fdec_base = symbol * params * ret_description * stms
[@@deriving show, eq]

type fdec = fdec_base pos_ast
[@@deriving show, eq]

type fdecs = fdec list
[@@deriving show, eq]

type fact_module = Module of fdecs
[@@deriving show, eq]