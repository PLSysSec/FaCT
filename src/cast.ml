open Pos
open Ast

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

type fentry = { f_rvt:var_type; f_args:description list }

type venv = description Env.env [@printer Env.pp_env]
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

type boolmask' =
  | TRUE
  | FALSE
and boolmask = boolmask' pos_ast [@@deriving show]

and cexpr_base =
  | CBoolean of bool
  | CNumber of int
  | CMask of boolmask
  | CVar of symbol
  | CArrAccess of symbol * cexpr
  | CArrComprehension of base_type * size * base_type * symbol * cexpr
  | CArrView of symbol * cexpr * size
  | CUnOp of unop * cexpr
  | CBinOp of binop * cexpr * cexpr
  | CTernaryOp of cexpr * cexpr * cexpr
  | CRef of symbol
  | CFunCall of symbol * cexprs
[@@deriving show, eq]

and cexpr' = cexpr_base * description
[@@deriving show]

and cexpr = cexpr' pos_ast [@@deriving show]

and cexprs = cexpr list

and cstm_base =
  | CVarDec of symbol * description * cexpr
  | CVarAssign of symbol * cexpr
  | CArrAssign of symbol * cexpr * cexpr
  | CFor of base_type * symbol * cexpr * cexpr * cstms
  | CReturn of cexpr
[@@deriving show, eq]

and cstm = cstm_base pos_ast
[@@deriving show, eq]

and cstms = cstm list

and block = Block of venv * mem * cstms
[@@deriving show]

and ret_expr = cexpr

and cfdec_base = {
  name:symbol;
  params:params;
  ret_description:ret_description;
  cstms:cstms
} [@@deriving show, eq]

and cfdec = cfdec_base pos_ast [@@deriving show]

and cfdecs = cfdec list

and cmodule = CModule of fenv * fdecs
[@@deriving show]

let ltk vt k = { ty=vt.v_ty; lbl=vt.v_lbl; kind=k }
let vtk lt = { v_ty=lt.ty; v_lbl=lt.lbl }
