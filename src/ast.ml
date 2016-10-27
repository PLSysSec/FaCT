
type constantc_module = FDec of fdec list

and fdec = FunctionDec of string * param list * constantc_type * stm list

and constantc_type =
  | Int
  | Bool
  | ByteArr

and param = { name: string; ty: constantc_type }

and stm =
  | VarDec of string * constantc_type * expr
  | Assign of string * expr
  | If of expr * stm list * stm list
  | While of expr * stm list
  | Return of expr

and expr =
  | VarExp of string
  | Unop of unop * expr
  | BinOp of binop * expr * expr
  | Primitive of primitive
  | CallExp of string * expr list

and unop =
  | B_Not
  | Negate

and binop =
  | Plus
  | Minus
  | GT
  | B_And
  | B_Or

and primitive = Number of int
              | ByteArray of string
              | Boolean of bool


let ty_to_string = function
  | Int -> "Int"
  | Bool -> "Bool"
  | ByteArr -> "ByteArr"
