open Ast

type ventry = { v_ty: constantc_type }
type fentry = { f_ty: constantc_type; f_args: constantc_type list }

type entry =
  | VarEntry of ventry
  | FunEntry of fentry

type env = (string,entry) Hashtbl.t

val venv: env
