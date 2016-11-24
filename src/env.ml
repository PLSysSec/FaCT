open Stdlib

type ventry = { v_ty: Ast.ctype }
type fentry = { f_ty: Ast.ctype; f_args: Ast.ctype list }

type entry =
  | VarEntry of ventry
  | FunEntry of fentry

type env = (string,entry) Hashtbl.t

let venv =
  let v = Hashtbl.create 10 in
  let add_fun {name=n; ret_ty=ret; args_ty=args} =
    Hashtbl.add v n (FunEntry {f_ty=ret; f_args=args}) in
  let _ = List.map add_fun stdlib_funs in
  v
