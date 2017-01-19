open Llvm
open Ast

exception Error of string

type stdlib_desc =
  {name:string; ret_ty:Ast.ctype; args_ty:Ast.labeled_type list}

let stdlib_funs = [
]

let codegen_stdlib ctx m = function
  | f -> raise (Error ("Unknown function:\t" ^ f))
