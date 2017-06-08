open Pos

type size = int [@@deriving show, eq]
type var_name = string [@@deriving show, eq]
type fun_name = string [@@deriving show, eq]

type mutability' =
  | Const
  | Mut
[@@deriving show, eq]
and mutability = mutability' pos_ast [@@deriving show]

and label' =
  | Public
  | Secret
[@@deriving show, eq]
and label = label' pos_ast [@@deriving show]

and base_type' =
  | Uint of size
  | Int of size
  | Bool of bool
[@@deriving show, eq]
and base_type = base_type' pos_ast [@@deriving show]

and ref_type' =
  | Ref of base_type
[@@deriving show, eq]
and ref_type = ref_type' pos_ast [@@deriving show]

and lexpr' =
  | LIntLiteral of int
  | LVariable of string
  | LLength of string
[@@deriving show, eq]
and lexpr = lexpr' pos_ast [@@deriving show]

and array_type' =
  | ArrayAT of base_type * lexpr
[@@deriving show, eq]
and array_type = array_type' pos_ast [@@deriving show]

and expr_type' =
  | BaseET of base_type * label
  | ArrayET of array_type * label * mutability
[@@deriving show, eq]
and expr_type = expr_type' pos_ast [@@deriving show]

and variable_type' =
  | RefVT of ref_type * label * mutability
  | ArrayVT of array_type * label * mutability
[@@deriving show, eq]
and variable_type = variable_type' pos_ast [@@deriving show]

and expr' =
  | True
  | False
  | IntLiteral of int
  | Variable of var_name
  | ArrayGet of var_name * expr
  | ArrayLen of var_name
  | IntCast of base_type * expr
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | TernOp of expr * expr * expr
  | FnCall of fun_name * arg_expr
  | Declassify of expr
[@@deriving show, eq]
and expr = expr' pos_ast [@@deriving show]

and unop =
  | Neg
  | LogicalNot
  | BitwiseNot
[@@deriving show, eq]

and binop =
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

and array_expr' =
  | ArrayZeros of lexpr
  | ArrayCopy of var_name
  | ArrayView of var_name * expr * lexpr
  | ArrayComp of base_type * lexpr * var_name * expr
[@@deriving show, eq]
and array_expr = array_expr' pos_ast [@@deriving show]

and arg_exprs = arg_expr list [@@deriving show, eq]

and arg_expr' =
  | ByValue of expr
  | ByArray of array_expr
  | ByRef of var_name
[@@deriving show, eq]
and arg_expr = arg_expr' pos_ast [@@deriving show]

and array_index = expr [@@deriving show, eq]
and cond = expr [@@deriving show, eq]
and thenstms = statements [@@deriving show, eq]
and elsestms = statements [@@deriving show, eq]
and statements = statement list [@@deriving show, eq]
and low_expr = expr [@@deriving show, eq]
and high_expr = expr [@@deriving show, eq]

and statement' =
  | BaseDec of var_name * variable_type * expr
  | ArrayDec of var_name * variable_type * array_expr
  | BaseAssign of var_name * expr
  | ArrayAssign of var_name * array_index * expr
  | If of cond * thenstms * elsestms
  | For of var_name * low_expr * high_expr * statements
  | Return of expr
[@@deriving show, eq]
and statement = statement' pos_ast [@@deriving show]

and arg' =
  | Arg of variable_type
[@@deriving show, eq]
and arg = arg' pos_ast [@@deriving show]

and args = arg list [@@deriving show, eq]
and body = statements [@@deriving show, eq]

and function_dec' =
  | FunDec of fun_name * expr_type * args * body
[@@deriving show, eq]
and function_dec = function_dec' pos_ast [@@deriving show]

and function_decs = function_dec list
[@@deriving show, eq]

and fact_module =
  | Module of function_decs
[@@deriving show, eq]

(*
  Auxiliary Functions
*)

val is_int : base_type -> bool
