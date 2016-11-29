open Env

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string

val tc_expr: env -> Ast.expr -> Ast.ctype
val tc_stm: Ast.ctype -> env -> Ast.stm -> unit
val tc_stms: Ast.ctype -> env -> Ast.stm list -> unit
val tc_fdec: env -> Ast.fdec -> unit
val tc_module: Ast.constantc_module -> unit
