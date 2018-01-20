open Tast
open Pos
open Llvm

exception DebugFunError

type mode = DEV | PROD
[@@deriving show]

let pos : pos = {file=""; line=0; lpos=0; rpos=0}

let print_fun () =
  let name = make_ast pos "print" in
  let var_name = make_ast pos "input" in
  let var_type = make_ast pos String in
  let label = make_ast pos (Fixed Public) in
  let mutability = make_ast pos Const in
  let var_expr_type = make_ast pos (RefVT(var_type,label,mutability)) in
  let param = make_ast pos (Param(var_name,var_expr_type)) in
  let fdec = make_ast pos (DebugFunDec(name,None,[param])) in
  name,fdec


let print_int_fun () =
  let name = make_ast pos "print_int" in
  let int_var_name = make_ast pos "int_input" in
  let string_var_name = make_ast pos "string_input" in
  let int_type = make_ast pos (Int 64) in
  let string_type = make_ast pos String in
  let label = make_ast pos (Fixed Public) in
  let mutability = make_ast pos Const in
  let int_expr_type = make_ast pos (RefVT(int_type,label,mutability)) in
  let string_expr_type = make_ast pos (RefVT(string_type,label,mutability)) in
  let int_param = make_ast pos (Param(int_var_name,int_expr_type)) in
  let string_param = make_ast pos (Param(string_var_name,string_expr_type)) in
  let fdec = make_ast pos (DebugFunDec(name,None,[string_param;int_param])) in
  name,fdec

let codegen_print llcontext llmod =
  let printf_ty = var_arg_function_type (i32_type llcontext) [| pointer_type (i8_type llcontext) |] in
  declare_function "printf" printf_ty llmod

let codegen_print_int llcontext llmod =
  let printf_ty = var_arg_function_type (i32_type llcontext) [| pointer_type (i8_type llcontext); (i64_type llcontext) |] in
  declare_function "printf" printf_ty llmod

let make_fenv () =
  let env = Env.new_env () in
  let print_name,print_proto = print_fun () in
  let print_int_name,print_int_proto = print_int_fun () in
    Env.add_var env print_name print_proto;
    Env.add_var env print_int_name print_int_proto;
    env


let codegen_proto llcontext llmod var_name =
  match var_name.data with
    | "print" -> codegen_print llcontext llmod
    | "print_int" -> codegen_print_int llcontext llmod
    | _ -> raise DebugFunError

let functions = [
  print_fun ();
  print_int_fun ();
]
