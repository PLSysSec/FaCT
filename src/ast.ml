open Pos

type bigint = Z.t [@printer pp_bigint][@@deriving show]
let pp_bigint = Z.pp_print
let equal_bigint = Z.equal

type ctype =
  | Bool
  | Int of bigint
  | UInt of bigint
[@@deriving show, eq]

type label =
  | Public
  | Secret
  | Unknown
[@@deriving show, eq]

(* "kind" actually means "storage" here *)
type kind =
  | Val
  | Ref
  | Arr of bigint
[@@deriving show, eq]

type var_type = { v_ty:ctype; v_lbl:label }
[@@deriving show, eq]

type labeled_type = { ty:ctype; label:label; kind:kind }
[@@deriving show, eq]

type constantc_module = CModule of fdec list
[@@deriving show]

and fdec' = { name:string; params:param list; rvt:var_type; body:stm list }
[@@deriving show]
and fdec = fdec' pos_ast [@@deriving show]

and param' = { name:string; lt:labeled_type }
[@@deriving show]
and param = param' pos_ast [@@deriving show]

and arrinit' =
  | ZeroArray
[@@deriving show]
and arrinit = arrinit' pos_ast [@@deriving show]

and stm' =
  | VarDec of string * var_type * expr
  | ArrDec of string * var_type * bigint * arrinit
  | Assign of string * expr
  | ArrAssign of string * expr * expr
  | If of expr * stm list * stm list
  | For of string * ctype * expr * expr * stm list
  | Return of expr
[@@deriving show]
and stm = stm' pos_ast [@@deriving show]

and expr' =
  | VarExp of string
  | ArrExp of string * expr
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | Primitive of primitive
  | CallExp of string * arg list
[@@deriving show]
and expr = expr' pos_ast [@@deriving show]

and arg' =
  | ValArg of expr
  | RefArg of string (* when parsing, kind is Val only if explicitly given; it parses as a ValArg of VarExp if not *)
  | ArrArg of string (* TODO should allow slicing *)
[@@deriving show]
and arg = arg' pos_ast [@@deriving show]

and unop' =
  | Neg
  | L_Not
  | B_Not
[@@deriving show]
and unop = unop' pos_ast [@@deriving show]

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
[@@deriving show]
and binop = binop' pos_ast [@@deriving show]

and primitive' =
  | Number of bigint
  | Boolean of bool
[@@deriving eq]
and primitive = primitive' pos_ast [@@deriving show]

let ltk vt k = { ty=vt.v_ty; label=vt.v_lbl; kind=k }
let vtk lt = { v_ty=lt.ty; v_lbl=lt.label }

let rec ty_to_string = function
  | Bool -> "bool"
  | Int size -> "int" ^ show_bigint size
  | UInt size -> "uint" ^ show_bigint size
