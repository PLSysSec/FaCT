type ctype =
  | Int
  | ByteArr of int
[@@deriving show]

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
  | LeftShift
  | RightShift
[@@deriving show]

and unop =
  | BitNot
[@@deriving show]

and primitive =
  | Number of int
  | ByteArray of int list
[@@deriving show]

and expr =
  | VarExp of string
  | ArrExp of string * expr
  | Primitive of primitive
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | CallExp of string * expr list
[@@deriving show]

and stm =
  | VarDec of string * ctype * expr
  | Assign of string * expr
  | ArrAssign of string * expr * expr
  | For of string * primitive * primitive * stm list
[@@deriving show]

and fdec = FunctionDec of string * param list * ctype * stm list * expr
[@@deriving show]

and param = { name: string; ty: ctype }
[@@deriving show]

and cmodule = CModule of fdec list
[@@deriving show]
