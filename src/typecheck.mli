open Env

exception NotImplemented
exception FunctionNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string
exception InternalCompilerError of string

(*val tc_expr: env -> Ast.labeled_type -> Ast.expr -> Ast.expr
val tc_stm: Ast.labeled_type -> env -> string -> Ast.stm -> Ast.stm
val tc_stms: Ast.labeled_type -> env -> Ast.stm list -> Ast.stm list
val tc_fdec: env -> Ast.fdec -> Ast.fdec

val tc_expr: env -> Ast.expr -> Ast.expr

val lbl_can_flow: env -> Ast.labeled_type -> Ast.expr -> Ast.labeled_type
val lbl_can_pass: env -> Ast.labeled_type -> (Ast.arg' * Ast.labeled_type) -> Ast.labeled_type
val can_pass: env -> Ast.labeled_type -> (Ast.arg * Ast.labeled_type option) -> unit
  *)
val tc_module: Ast.constantc_module -> Tast.tconstantc_module
