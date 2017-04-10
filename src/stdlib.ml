open Err
open Llvm
open Ast

exception Error of string

type stdlib_desc =
  { name:string; ret_ty:Ast.ctype; args_ty:Ast.description list }

let stdlib_funs = [
]

let codegen_stdlib ctx m = function
  | f -> raise_error_np (FunctionNotDefined f)
