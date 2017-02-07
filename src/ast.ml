open Lexing

(* all this pos stuff should be moved to lexing *)

type pos = { file:string; line:int; lpos:int; rpos:int }
[@@deriving show, eq]

type 'a pos_ast = { pos:pos; data:'a }
[@@deriving show, eq]

let to_pos ?buf:(b=None)
    { pos_fname=f; pos_lnum=l; pos_bol=bl; pos_cnum=c } =
  match b with
  | None -> { file=f; line=l; lpos=bl; rpos=c }
  | Some lb ->
    let start = (lexeme_start lb) - lb.lex_curr_p.pos_bol in
    let ends = (lexeme_end lb) - lb.lex_curr_p.pos_bol in
    { file=f; line=l; lpos=start+1; rpos=ends+1 }

let make_pos pos data = { pos=(to_pos pos); data=data }
let make_ast pos data = { pos=pos; data=data }

let pos_string { file=f; line=l; lpos=lp; rpos=rp } =
  "file " ^ f ^ ", line " ^ string_of_int(l) ^
  ", from " ^ string_of_int(lp) ^ "-" ^ string_of_int(rp)

let repack fn = fun pa -> { pos=pa.pos; data=(fn pa) }
let unpack fn = fun { pos; data } -> fn data
let posmap fn = fun { pos; data } -> { pos=pos; data=(fn data) }

type bigint = Z.t [@@deriving eq]
let pp_bigint = Z.pp_print

type constantc_module = CModule of fdec list
[@@deriving show, eq]

and ctype =
  | Bool
  | Int of bigint
  | UInt of bigint
  | Array of { ty:ctype; size:bigint }
[@@deriving show, eq]

and label =
  | Public
  | Secret
  | Unknown
[@@deriving show, eq]

and kind =
  | Ref
  | Val
  | Out
[@@deriving show, eq]

and labeled_type = { ty:ctype; label:label; kind:kind }
[@@deriving show, eq]

and fdec' = { name:string; params:param list; rty:ctype; rlbl:label; body:stm list }
[@@deriving show, eq]
and fdec = fdec' pos_ast [@@deriving show, eq]

and param' = { name:string; lt:labeled_type }
[@@deriving show, eq]
and param = param' pos_ast [@@deriving show, eq]

and stm' =
  | VarDec of string * labeled_type * expr
  | Assign of string * expr
  | ArrAssign of string * expr * expr
  | If of expr * stm list * stm list
  | For of string * ctype * expr * expr * stm list
  | Return of expr
[@@deriving show, eq]
and stm = stm' pos_ast [@@deriving show, eq]

and expr' =
  | VarExp of string
  | ArrExp of string * expr
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | Primitive of primitive
  | CallExp of string * arg list
[@@deriving show, eq]
and expr = expr' pos_ast [@@deriving show, eq]

and arg' =
  | ValArg of expr
  | VarArg of kind * string (* when parsing, kind is Val only if explicitly given; it parses as a ValArg of VarExp if not *)
  | ArrArg of kind * string (* TODO should allow slicing *)
[@@deriving show, eq]
and arg = arg' pos_ast [@@deriving show, eq]

and unop' =
  | Neg
  | L_Not
  | B_Not
[@@deriving show, eq]
and unop = unop' pos_ast [@@deriving show, eq]

and binop' =
  | Plus
  | Minus
  | Multiply
  | Equal
  | NEqual
  | GT
  | GTE
  | LT
  | LTE
  | L_And
  | L_Or
  | B_And
  | B_Or
  | B_Xor
  | LeftShift
  | RightShift
[@@deriving show, eq]
and binop = binop' pos_ast [@@deriving show, eq]

and primitive' =
  | Number of Z.t [@printer Z.pp_print]
  | Boolean of bool
  | ArrayLiteral of expr list
[@@deriving eq]
and primitive = primitive' pos_ast [@@deriving show, eq]

let show_primitive' x = "hi"

let rec ty_to_string = function
  | Bool -> "bool"
  | Int size -> "int" ^ Z.to_string size
  | UInt size -> "uint" ^ Z.to_string size
  | Array t -> (ty_to_string t.ty) ^ "[" ^ (Z.to_string t.size) ^ "]"
