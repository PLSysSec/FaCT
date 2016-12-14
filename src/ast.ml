open Lexing

type pos = { file:string; line:int; lpos:int; rpos:int }
[@@deriving show]

type constantc_module = CModule of fdec list
[@@deriving show]

and fdec = FunctionDec of string * param list * ctype * stm list * pos
[@@deriving show]

and ctype =
  | Int
  | Bool
  | ByteArr of int
[@@deriving show]

and param = { name: string; ty: ctype }
[@@deriving show]

and stm =
  | VarDec of string * ctype * expr
  | Assign of string * expr
  | ArrAssign of string * expr * expr
  | If of expr * stm list * stm list
  | For of string * primitive * primitive * stm list
  | Return of expr
[@@deriving show]

and expr =
  | VarExp of string
  | ArrExp of string * expr
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | Primitive of primitive
  | CallExp of string * expr list
[@@deriving show]

and unop =
  | B_Not
[@@deriving show]

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
  | B_And
  | B_Or
  | LeftShift
  | RightShift
[@@deriving show]

and primitive =
  | Number of int
  | Boolean of bool
  | ByteArray of int list
[@@deriving show]

let ty_to_string = function
  | Int -> "Int"
  | Bool -> "Bool"
  | ByteArr _ -> "ByteArr"
