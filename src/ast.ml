
type constantc_module = CModule of fdec list

and fdec = FunctionDec of string * param list * ctype * stm list

and ctype =
  | Int
  | Bool
  | ByteArr

and param = { name: string; ty: ctype }

and stm =
  | VarDec of string * ctype * expr
  | Assign of string * expr
  | If of expr * stm list * stm list
  | For of string * primitive * primitive * stm list (* TODO typecheck primitives: Number only *)
  | Return of expr

and expr =
  | VarExp of string
  | Unop of unop * expr
  | BinOp of binop * expr * expr
  | Primitive of primitive
  | CallExp of string * expr list

and unop =
  | B_Not

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
