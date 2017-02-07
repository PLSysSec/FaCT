type fentry = { f_rty:Ast.ctype; f_rlbl:Ast.label; f_args:Ast.labeled_type list }

type entry =
  | VarEntry of Ast.labeled_type ref
  | FunEntry of fentry

type env = (string,entry) Hashtbl.t
val pp_env : Format.formatter -> env -> unit
val equal_env : env -> env -> bool

val venv : env

val get_var : env -> string -> Ast.labeled_type
val get_fn : env -> string -> fentry
val update_label : env -> string -> Ast.label -> unit

(*val print_env : env -> unit*)
