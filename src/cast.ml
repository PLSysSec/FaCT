type ctype =
  | Int (* have (u)int[8|16|32|64]? *)
  | ByteArr

and binop =
  | Plus
  | Minus
  | Mult (* figure out if this is truly const *)
  | GT (* do comparisons short-circuit? *)
  | Eq (* more hardware question marks *)
  | Neq
  | BitAnd
  | BitOr

and unop =
  | BitNot

and primitive =
  | Number of int32
  | ByteArray of string

and expr =
  | VarExp of string
  | Primitive of primitive
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | CallExp of string * expr list (* because we're not monsters *)

and stm =
  | VarDec of string * ctype * expr
  | Assign of string * expr
  | For of string * primitive * primitive * stm list

and fdec = FunctionDec of string * param list * ctype * stm list * expr

and param = { name: string; ty: ctype }

and cmodule = CModule of fdec list
