open Lexing

type pos = { file:string; line:int; lpos:int; rpos:int }
[@@deriving show, eq]

let to_pos ?buf:(b=None)
    { pos_fname=f; pos_lnum=l; pos_bol=bl; pos_cnum=c } =
  match b with
  | None -> { file=f; line=l; lpos=bl; rpos=c }
  | Some lb ->
    let start = (lexeme_start lb) - lb.lex_curr_p.pos_bol in
    let ends = (lexeme_end lb) - lb.lex_curr_p.pos_bol in
    { file=f; line=l; lpos=start+1; rpos=ends+1 }

let pos_string { file=f; line=l; lpos=lp; rpos=rp } =
  "file " ^ f ^ ", line " ^ string_of_int(l) ^
  ", from " ^ string_of_int(lp) ^ "-" ^ string_of_int(rp)

type constantc_module = CModule of fdec list
[@@deriving show, eq]

and fdec = FunctionDec of string * param list * labeled_type * stm list * pos
[@@deriving show, eq]

and ctype =
  | Int32
  | Int16
  | Int8
  | UInt32
  | UInt16
  | UInt8
  | Int
  | Bool
  | ByteArr of int
[@@deriving show, eq]

and label =
  | Public
  | Secret
[@@deriving show, eq]

and kind =
  | Ref
  | Val
  | Out
[@@deriving show, eq]

and labeled_type = { ty:ctype; label:label option; kind:kind }
[@@deriving show, eq]

and param = { name:string; lt:labeled_type; p:pos }
[@@deriving show, eq]

and stm =
  | VarDec of string * labeled_type * expr * pos
  | Assign of string * expr * pos
  | ArrAssign of string * expr * expr * pos
  | If of expr * stm list * stm list * pos
  | For of string * primitive * primitive * stm list * pos
  | Return of expr * pos
[@@deriving show, eq]

and expr =
  | VarExp of string * pos
  | ArrExp of string * expr * pos
  | UnOp of unop * expr * pos
  | BinOp of binop * expr * expr * pos
  | Primitive of primitive * pos option
  | CallExp of string * expr list * pos
[@@deriving show, eq]

and unop =
  | Neg of pos
  | L_Not of pos
  | B_Not of pos
[@@deriving show, eq]

and binop =
  | Plus of pos
  | Minus of pos
  | Multiply of pos
  | Equal of pos
  | NEqual of pos
  | GT of pos
  | GTE of pos
  | LT of pos
  | LTE of pos
  | L_And of pos
  | L_Or of pos
  | B_And of pos
  | B_Or of pos
  | B_Xor of pos
  | LeftShift of pos
  | RightShift of pos
[@@deriving show, eq]

and primitive =
  | Number of int
  | Boolean of bool
  | ByteArray of int list
[@@deriving show, eq]

let ty_to_string = function
  | Int32 _ -> "Int32"
  | Int16 _ -> "Int16"
  | Int8 _ -> "Int8"
  | UInt32 _ -> "UInt32"
  | UInt16 _ -> "UInt16"
  | UInt8 _ -> "UInt8"
  | Bool _ -> "Bool"
  | ByteArr _ -> "ByteArr"
  | Int _ -> "Int"