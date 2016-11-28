
type ventry = { v_ty: Ast.ctype }
type fentry = { f_ty: Ast.ctype; f_args: Ast.ctype list }

type entry =
  | VarEntry of ventry
  | LoopEntry of ventry
  | FunEntry of fentry

type env = (string,entry) Hashtbl.t

val venv: env
