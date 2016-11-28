type ctype =
  | Int (* have (u)int[8|16|32|64]? *)
  | ByteArr of int
[@@deriving show]

and binop =
  | Plus
  | Minus
  | Mult (* figure out if this is truly const *)
  | GT (* do comparisons short-circuit? *)
  | Eq (* more hardware question marks *)
  | Neq
  | BitAnd
  | BitOr
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
  | CallExp of string * expr list (* because we're not monsters *)
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
