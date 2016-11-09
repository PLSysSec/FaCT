open Ast
open Env

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string

val tc_expr: env -> expr -> ctype
val tc_stm: ctype -> env -> stm -> unit
val tc_stms: ctype -> env -> stm list -> unit
val tc_fdec: env -> fdec -> unit
val tc_module: constantc_module -> unit
