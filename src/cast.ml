type ctype =
  | Int32
  | Int16
  | Int8
  | UInt32
  | UInt16
  | UInt8
  | BoolMask
[@@deriving show]

and label =
  | Public
  | Secret

(* "kind" here means "storage" *)
and kind =
  | Ref
  | Val
  | Arr of int

and var_type = { v_ty:ctype; v_lbl:label }

and labeled_type = { ty:ctype; lbl:label; kind:kind }

and binop =
  | Plus
  | Minus
  | Mult
  | GT
  | GTE
  | LT
  | LTE
  | Eq
  | Neq
  | BitAnd
  | BitOr
  | BitXor
  | LeftShift
  | RightShift
[@@deriving show]

and unop =
  | Neg
  | BitNot
[@@deriving show]

and boolmask =
  | TRUE
  | FALSE

and primitive =
  | Number of int
  | Mask of boolmask
[@@deriving show]

and expr = { e:expr_base; e_ty:ctype }
[@@deriving show]

and expr_base =
  | VarExp of string
  | ArrExp of string * expr
  | Primitive of primitive
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | CallExp of string * expr list
[@@deriving show]

and arrinit =
  | ZeroArray
[@@deriving show]

and stm =
  | VarDec of string * var_type * expr
  | ArrDec of string * var_type * int * arrinit
  | Assign of string * expr
  | ArrAssign of string * expr * expr
  | For of string * expr * expr * stm list
(*  | XXX PublicIf of blah * blah * blah *)
(*  | XXX PublicRet of blah * blah * blah *)
[@@deriving show]

and fdec = FunctionDec of string * param list * var_type * stm list * expr
[@@deriving show]

and param = { name: string; lt: labeled_type }
[@@deriving show]

and cmodule = CModule of fdec list
[@@deriving show]

let ltk vt k = { ty=vt.v_ty; lbl=vt.v_lbl; kind=k }
let vtk lt = { v_ty=lt.ty; v_lbl=lt.lbl }
