
type constantc_module = CModule of fdec list

and fdec = FunctionDec of string * param list * ctype * stm list

and ctype =
  | Int
  | Bool
  | ByteArr of int

and param = { name: string; ty: ctype }

and stm =
  | VarDec of string * ctype * expr
  | Assign of string * expr
  | ArrAssign of string * expr * expr
  | If of expr * stm list * stm list
  | For of string * primitive * primitive * stm list
  | Return of expr

and expr =
  | VarExp of string
  | ArrExp of string * expr
  | Unop of unop * expr
  | BinOp of binop * expr * expr
  | Primitive of primitive
  | CallExp of string * expr list

and unop =
  | B_Not

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

and primitive = Number of int
              | Boolean of bool
              | ByteArray of int list

let ty_to_string = function
  | Int -> "Int"
  | Bool -> "Bool"
  | ByteArr _ -> "ByteArr"
