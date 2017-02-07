exception VariableNotDefined of string
exception FunctionNotDefined of string
exception NotImplemented
exception TypeError of string

type fentry = { f_rty:Ast.ctype; f_rlbl:Ast.label; f_args:Ast.labeled_type list }

type entry =
  | VarEntry of Ast.labeled_type ref
  | FunEntry of fentry

type env = (string,entry) Hashtbl.t

val venv : env

val get_var : env -> string -> Ast.labeled_type
val get_fn : env -> string -> fentry
val update_label : env -> string -> Ast.label -> unit
val update_fn : env -> Tast.tfdec -> unit

(*val print_env : env -> unit*)
