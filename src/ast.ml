open Pos

type bigint = Z.t [@printer pp_bigint][@@deriving show]
let pp_bigint = Z.pp_print
let equal_bigint = Z.equal

type constantc_module = CModule of fdec list
[@@deriving show, eq]

and ctype =
  | Bool
  | Int of bigint
  | UInt of bigint
[@@deriving show, eq]

and label =
  | Public
  | Secret
  | Unknown
[@@deriving show, eq]

(* "kind" actually means "storage" here *)
and kind =
  | Val
  | Ref
  | Arr of bigint
[@@deriving show, eq]

and var_type = { v_ty:ctype; v_lbl:label }
[@@deriving show, eq]

and labeled_type = { ty:ctype; label:label; kind:kind }
[@@deriving show, eq]

and fdec' = { name:string; params:param list; rvt:var_type; body:stm list }
[@@deriving show, eq]
and fdec = fdec' pos_ast [@@deriving show, eq]

and param' = { name:string; lt:labeled_type }
[@@deriving show, eq]
and param = param' pos_ast [@@deriving show, eq]

and arrinit' =
  | ZeroArray
[@@deriving show, eq]
and arrinit = arrinit' pos_ast [@@deriving show, eq]

and stm' =
  | VarDec of string * var_type * expr
  | ArrDec of string * var_type * bigint * arrinit
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
  | RefArg of string (* when parsing, kind is Val only if explicitly given; it parses as a ValArg of VarExp if not *)
  | ArrArg of string (* TODO should allow slicing *)
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
  | Number of bigint
  | Boolean of bool
[@@deriving eq]
and primitive = primitive' pos_ast [@@deriving show, eq]

let ltk vt k = { ty=vt.v_ty; label=vt.v_lbl; kind=k }
let vtk lt = { v_ty=lt.ty; v_lbl=lt.label }

let rec ty_to_string = function
  | Bool -> "bool"
  | Int size -> "int" ^ show_bigint size
  | UInt size -> "uint" ^ show_bigint size
