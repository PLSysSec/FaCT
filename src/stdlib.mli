open Ast
open Llvm

type stdlib_desc =
  {name:string; ret_ty:constantc_type; args_ty:constantc_type list}

val stdlib_funs : stdlib_desc list
val codegen_stdlib : llcontext -> llmodule -> string -> unit
