open Llvm
open Ast

exception Error of string

type stdlib_desc =
  {name:string; ret_ty:constantc_type; args_ty:constantc_type list}

let stdlib_funs = [
  {name="printf"; ret_ty=Int; args_ty=[ByteArr]}
]

let codegen_stdlib ctx m = function
  | "printf" ->
    let printf_ty =
      var_arg_function_type (i32_type ctx) [| pointer_type (i8_type ctx) |] in
    let _ = declare_function "printf" printf_ty m in ()
  | f -> raise (Error ("Unknown function:\t" ^ f))
