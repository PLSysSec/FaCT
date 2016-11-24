open Llvm

type stdlib_desc =
  {name:string; ret_ty:Ast.ctype; args_ty:Ast.ctype list}

val stdlib_funs : stdlib_desc list
val codegen_stdlib : llcontext -> llmodule -> string -> unit
