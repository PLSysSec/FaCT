
type constantc_module = FDec of fdec list

and fdec = FunctionDec of string * param list * constantc_type * stm list

and constantc_type =
  | Int
  | Bool
  | ByteArr of int

and param = { name: string; ty: constantc_type }

and stm =
  | VarDec of string * constantc_type * expr
  | Assign of string * expr
  | ArrAssign of string * int * expr
  | If of expr * stm list * stm list
  | While of expr * stm list
  | Return of expr

and expr =
  | VarExp of string
  | ArrExp of string * int (* name of array, index *)
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
              | Boolean of bool
              | ByteArray of string

let ty_to_string = function
  | Int -> "Int"
  | Bool -> "Bool"
  | ByteArr _ -> "ByteArr"
