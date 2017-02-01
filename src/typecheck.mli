open Env

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string
exception FunctionNotDefined of string

val tc_expr: env -> Ast.labeled_type -> Ast.expr -> Ast.expr
val tc_stm: Ast.labeled_type -> env -> string -> Ast.stm -> Ast.stm
val tc_stms: Ast.labeled_type -> env -> Ast.stm list -> Ast.stm list
val tc_fdec: env -> Ast.fdec -> Ast.fdec
val tc_module: Ast.constantc_module -> unit
