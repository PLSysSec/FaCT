open Llvm
open Ast

exception Error of string

type stdlib_desc =
  {name:string; ret_ty:constantc_type; args_ty:constantc_type list}

let stdlib_funs = [
  (* RIP printf *)
]

let codegen_stdlib ctx m = function
  | f -> raise (Error ("Unknown function:\t" ^ f))
