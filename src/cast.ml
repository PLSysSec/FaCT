open Err

type ctype =
  | Int32
  | Int16
  | Int8
  | UInt32
  | UInt16
  | UInt8
  | BoolMask
[@@deriving show]

and label =
  | Public
  | Secret

(* "kind" here means "storage" *)
and kind =
  | Val
  | Ref
  | Arr of int

and var_type = { v_ty:ctype; v_lbl:label }

and labeled_type = { ty:ctype; lbl:label; kind:kind }

type fentry = { f_rvt:var_type; f_args:labeled_type list }
[@@deriving show]

type entry =
  | VarEntry of labeled_type
  | FunEntry of fentry
[@@deriving show]

type env = (string,entry) Hashtbl.t
let pp_env fmt venv = Format.pp_print_text fmt "venv"

type mem = (string,Llvm.llvalue) Hashtbl.t
let pp_mem fmt mem = Format.pp_print_text fmt "mem"

type binop =
  | Plus
  | Minus
  | Mult
  | GT
  | GTE
  | LT
  | LTE
  | Eq
  | Neq
  | BitAnd
  | BitOr
  | BitXor
  | LeftShift
  | RightShift
[@@deriving show]

and unop =
  | Neg
  | BitNot
[@@deriving show]

and boolmask =
  | TRUE
  | FALSE

and primitive =
  | Number of int
  | Mask of boolmask
[@@deriving show]

and expr = { e:expr_base; e_ty:ctype }
[@@deriving show]

and expr_base =
  | VarExp of string
  | ArrExp of string * expr
  | Primitive of primitive
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | CallExp of string * arg list
[@@deriving show]

and arg =
  | ValArg of expr
  | VarArg of string * labeled_type
  | ArrArg of string * labeled_type
[@@deriving show]

and arrinit =
  | ZeroArray
[@@deriving show]

and stm =
  | VarDec of string * var_type * expr
  | ArrDec of string * var_type * int * arrinit
  | Assign of string * expr
  | ArrAssign of string * expr * expr
  | For of string * ctype * expr * expr * block
(*  | XXX PublicIf of blah * blah * blah *)
(*  | XXX PublicRet of blah * blah * blah *)
[@@deriving show]

and block = { venv:env; mem:mem; body:stm list }
[@@deriving show]

and fdec = FunctionDec of string * param list * var_type * block * expr
[@@deriving show]

and param = { name: string; lt: labeled_type }
[@@deriving show]

and cmodule = CModule of fdec list
[@@deriving show]

let ltk vt k = { ty=vt.v_ty; lbl=vt.v_lbl; kind=k }
let vtk lt = { v_ty=lt.ty; v_lbl=lt.lbl }

let get_var venv v =
  try
    match Hashtbl.find venv v with
      | VarEntry lt -> lt
      | _ -> raise @@ errFoundNotVar v
  with
      Not_found -> raise @@ errVarNotDefined v

let get_arr venv v =
  try
    match Hashtbl.find venv v with
      | VarEntry lt ->
        (match lt.kind with
          | Arr _ -> { lt with kind=Ref }
          | _ -> raise @@ errFoundNotArr v)
      | _ -> raise @@ errFoundNotVar v
  with
      Not_found -> raise @@ errVarNotDefined v

let get_fn venv f =
  try
    match Hashtbl.find venv f with
      | FunEntry fentry -> fentry
      | _ -> raise @@ errFoundNotVar f
  with
    Not_found -> raise @@ errFnNotDefined f

let mem_var mem n =
  try Hashtbl.find mem n with
    | Not_found -> raise (VariableNotDefined ("Could not find var " ^ n ^ " in memory"))
