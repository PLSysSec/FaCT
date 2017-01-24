open Lexing

type pos = { file:string; line:int; lpos:int; rpos:int }
[@@deriving show]

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
[@@deriving show]

and fdec = FunctionDec of string * param list * labeled_type * stm list * pos
[@@deriving show]

and ctype =
  | Int
  | Bool
  | ByteArr of int
[@@deriving show]

and label =
  | Public
  | Secret

and kind =
  | Ref
  | Val
  | Out

and labeled_type = { ty:ctype; label:label option; kind:kind }
[@@deriving show]

and param = { name:string; lt:labeled_type; p:pos }
[@@deriving show]

and stm =
  | VarDec of string * labeled_type * expr * pos
  | Assign of string * expr * pos
  | ArrAssign of string * expr * expr * pos
  | If of expr * stm list * stm list * pos
  | For of string * primitive * primitive * stm list * pos
  | Return of expr * pos
[@@deriving show]

and expr =
  | VarExp of string * pos
  | ArrExp of string * expr * pos
  | UnOp of unop * expr * pos
  | BinOp of binop * expr * expr * pos
  | Primitive of primitive * pos option
  | CallExp of string * expr list * pos
[@@deriving show]

and unop =
  | Neg of pos
  | L_Not of pos
  | B_Not of pos
[@@deriving show]

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
[@@deriving show]

and primitive =
  | Number of int
  | Boolean of bool
  | ByteArray of int list
[@@deriving show]

let ty_to_string = function
  | Int _ -> "Int"
  | Bool _ -> "Bool"
  | ByteArr _ -> "ByteArr"
