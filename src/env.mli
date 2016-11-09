open Ast

type ventry = { v_ty: ctype }
type fentry = { f_ty: ctype; f_args: ctype list }

type entry =
  | VarEntry of ventry
  | FunEntry of fentry

type env = (string,entry) Hashtbl.t

val venv: env
