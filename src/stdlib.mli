open Ast
open Llvm

type stdlib_desc =
  {name:string; ret_ty:ctype; args_ty:ctype list}

val stdlib_funs : stdlib_desc list
val codegen_stdlib : llcontext -> llmodule -> string -> unit
