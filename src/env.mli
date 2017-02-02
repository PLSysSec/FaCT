exception FunctionNotFound of string
exception NotImplemented

type entry =
  | VarEntry of Ast.labeled_type ref
  | FunEntry of { f_ty:Ast.expr_type; f_args:Ast.labeled_type list }

type env = (string,entry) Hashtbl.t

val venv : env

val save_fn_ret_label : Ast.labeled_type -> string -> unit
val get_fn_ret_label : default:Ast.labeled_type -> string -> Ast.labeled_type

val update_label : env -> string -> Ast.label option -> unit

(*val print_env : env -> unit*)
