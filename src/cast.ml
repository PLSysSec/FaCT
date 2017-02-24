open Pos

type ctype =
  | Int of int
  | UInt of int
  | BoolMask
[@@deriving show, eq]

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

type venv = labeled_type Env.env [@printer Env.pp_env]
[@@deriving show]

type mem = Llvm.llvalue Env.env [@printer Env.pp_env]
[@@deriving show]

type fenv = (string,fentry) Hashtbl.t
let pp_fenv fmt fenv =
  let pp = Format.pp_print_text fmt in
    begin
      pp "{ ";
      Hashtbl.iter
        (fun k v -> pp (k ^ "; "))
        fenv;
      pp "}";
    end

type binop' =
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
and binop = binop' pos_ast [@@deriving show]

and unop' =
  | Neg
  | BitNot
[@@deriving show]
and unop = unop' pos_ast [@@deriving show]

and boolmask' =
  | TRUE
  | FALSE
and boolmask = boolmask' pos_ast [@@deriving show]

and primitive' =
  | Number of int
  | Mask of boolmask
[@@deriving show]
and primitive = primitive' pos_ast [@@deriving show]

and expr' = { e:expr_base; e_ty:ctype }
[@@deriving show]
and expr = expr' pos_ast [@@deriving show]

and expr_base' =
  | VarExp of string
  | ArrExp of string * expr
  | Primitive of primitive
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | CallExp of string * arg list
[@@deriving show]
and expr_base = expr_base' pos_ast [@@deriving show]

and arg' =
  | ValArg of expr
  | RefArg of string * var_type
  | ArrArg of string * var_type * int
[@@deriving show]
and arg = arg' pos_ast [@@deriving show]

and arrinit' =
  | UnsafeNoInit
[@@deriving show]
and arrinit = arrinit' pos_ast [@@deriving show]

and stm' =
  | VarDec of string * var_type * expr
  | ArrDec of string * var_type * int * arrinit
  | Assign of string * expr
  | ArrAssign of string * expr * expr
  | For of string * ctype * expr * expr * block
[@@deriving show]
and stm = stm' pos_ast [@@deriving show]

and block = { venv:venv; mem:mem; body:stm list }
[@@deriving show]

and fdec' = FunctionDec of string * param list * var_type * block * expr
[@@deriving show]
and fdec = fdec' pos_ast [@@deriving show]


and param' = { name: string; lt: labeled_type }
[@@deriving show]
and param = param' pos_ast [@@deriving show]

and cmodule = CModule of fenv * fdec list
[@@deriving show]

let ltk vt k = { ty=vt.v_ty; lbl=vt.v_lbl; kind=k }
let vtk lt = { v_ty=lt.ty; v_lbl=lt.lbl }
