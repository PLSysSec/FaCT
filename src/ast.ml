open Pos

type size = int [@@deriving show]

type var_name' = string [@@deriving show]
and var_name = var_name' pos_ast [@@deriving show]

type fun_name' = string [@@deriving show]
and fun_name = fun_name' pos_ast [@@deriving show]

type struct_name' = string [@@deriving show]
and struct_name = struct_name' pos_ast [@@deriving show]

type mutability' =
  | Const
  | Mut
[@@deriving show]
and mutability = mutability' pos_ast [@@deriving show]

and label' =
  | Public
  | Secret
  | Unknown
[@@deriving show]
and label = label' pos_ast [@@deriving show]

and base_type' =
  | UInt of size
  | Int of size
  | Bool
  | String
[@@deriving show]
and base_type = base_type' pos_ast [@@deriving show]

and lexpr' =
  | LExpression of expr
  | LUnspecified
[@@deriving show]
and lexpr = lexpr' pos_ast [@@deriving show]

and array_type' =
  | ArrayAT of base_type * lexpr
[@@deriving show]
and array_type = array_type' pos_ast [@@deriving show]

and expr_type' =
  | BaseET of base_type * label
  | ArrayET of array_type * label * mutability
[@@deriving show]
and expr_type = expr_type' pos_ast [@@deriving show]

and variable_type' =
  | RefVT of base_type * label * mutability
  | ArrayVT of array_type * label * mutability
  | StructVT of struct_name * mutability
[@@deriving show]
and variable_type = variable_type' pos_ast [@@deriving show]

and lvalue' =
  | Base of var_name
  | ArrayEl of lvalue * array_index
  | StructEl of lvalue * var_name
and lvalue = lvalue' pos_ast

and expr' =
  | True
  | False
  | IntLiteral of int
  | StringLiteral of string
  | Lvalue of lvalue
  | ArrayElLen of lvalue
  | Sizeof of struct_name (* to be implemented *)
  | IntCast of base_type * expr
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | TernOp of expr * expr * expr
  | FnCall of fun_name * arg_exprs
  | Declassify of expr
[@@deriving show]
and expr = expr' pos_ast [@@deriving show]

and unop =
  | Neg
  | LogicalNot
  | BitwiseNot
[@@deriving show]

and binop =
  | Plus
  | Minus
  | Multiply
  | Divide
  | Modulo
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
  | LeftRotate
  | RightRotate
[@@deriving show]

and array_expr' =
  | ArrayLit of expr list
  | ArrayVar of lvalue
  | ArrayZeros of lexpr
  | ArrayCopy of lvalue
  | ArrayView of lvalue * expr * lexpr
  | ArrayComp of base_type * lexpr * var_name * expr
  | ArrayNoinit of lexpr
[@@deriving show]
and array_expr = array_expr' pos_ast [@@deriving show]

and arg_exprs = arg_expr list [@@deriving show]

and arg_expr' =
  | ByValue of expr
  | ByArray of array_expr * mutability
  | ByRef of lvalue
[@@deriving show]
and arg_expr = arg_expr' pos_ast [@@deriving show]

and array_index = expr [@@deriving show]
and cond = expr [@@deriving show]
and thenstms = statements [@@deriving show]
and elsestms = statements [@@deriving show]
and statements = statement list [@@deriving show]
and low_expr = expr [@@deriving show]
and high_expr = expr [@@deriving show]

and statement' =
  | BaseDec of var_name * variable_type * expr
  | ArrayDec of var_name * variable_type * array_expr
  | Assign of lvalue * expr
  | If of cond * thenstms * elsestms
  | For of var_name * base_type * low_expr * high_expr * statements
  | VoidFnCall of fun_name * arg_exprs
  | Return of expr
  | VoidReturn
[@@deriving show]
and statement = statement' pos_ast [@@deriving show]

and param' =
  | Param of var_name * variable_type
[@@deriving show]
and param = param' pos_ast [@@deriving show]

and field' =
  | Field of var_name * variable_type
[@@deriving show]
and field = field' pos_ast [@@deriving show]

and params = param list [@@deriving show]
and body = statements [@@deriving show]
and fields = field list [@@deriving show]

and ret_type = expr_type option [@@deriving show]
and fn_type = { export : bool; inline : inline }
and inline =
  | Default
  | Always
  | Never

and function_dec' =
  | FunDec of fun_name * fn_type * ret_type * params * body
  | CExtern of fun_name * ret_type * params
[@@deriving show]
and function_dec = function_dec' pos_ast [@@deriving show]

and function_decs = function_dec list
[@@deriving show]

and struct_type' =
  | Struct of struct_name * fields
and struct_type = struct_type' pos_ast [@@deriving show]

and structs = struct_type list
[@@deriving show]

and fact_module =
  | Module of function_decs * structs
[@@deriving show]

(* Used to parse a top level value in the REPL *)
and top_level =
| FunctionDec of function_dec
| Statement of statement
| Expression of expr
[@@deriving show]
