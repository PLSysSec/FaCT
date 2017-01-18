open Env

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string
exception FunctionNotDefined of string

val tc_expr: env -> Ast.expr -> Ast.labeled_type
val tc_stm: Ast.labeled_type -> env -> string -> Ast.stm -> unit
val tc_stms: Ast.labeled_type -> env -> Ast.stm list -> string -> unit
val tc_fdec: env -> Ast.fdec -> unit
val tc_module: Ast.constantc_module -> unit
