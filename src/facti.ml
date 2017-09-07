open Llvm
open Llvm_executionengine
open Llvm_target
open Typecheck
open Pos
open Ctypes

let fake_pos = { file="JIT"; line=0; lpos=0; rpos=0 }

let count = ref 0

let expr_to_fun e =
  let ret_int = make_ast fake_pos (Ast.Int 32) in
  let pub = make_ast fake_pos Ast.Public in
  let ret_type = make_ast fake_pos (Ast.BaseET(ret_int, pub)) in 
  let name = ("fun_ptr" ^ (string_of_int !count)) in
  let fun_name = make_ast fake_pos name in
  count := !count + 1;
  let body = make_ast fake_pos (Ast.Return e) in
  name, make_ast fake_pos (Ast.FunDec(fun_name, (Some ret_type), [], [body]))

let rec repl ctx mod' builder execution_engine fenv venv cg_fenv =
  print_string "fact>"; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.expr_top Lexer.token lexbuf in
  print_string ((Ast.show_expr ast) ^ "\n");
  let name,ast' = expr_to_fun ast in
  let tast = tc_fdec fenv ast' in
  print_string ((Tast.show_function_dec tast) ^ "\n");
  let f = Codegen.codegen_fun ctx mod' builder cg_fenv tast in
  (* JIT the function, returning a function pointer. *)
  run_static_ctors execution_engine;
  let ty = Foreign.funptr (void @->returning int32_t) in
  let fun_ptr = get_function_address name ty execution_engine in
  print_string "Evaluated to:\n";
  print_string (Int32.to_string (fun_ptr ()));
  (*print_float (GenericValue.as_float Codegen.double_type result);*)
  print_newline ();
  let m = "Fact REPL " ^ (string_of_int !count) in
  let mod'' = create_module ctx m in
  remove_module mod' execution_engine;
  dispose_module mod';
  run_static_dtors execution_engine;
  let execution_engine' = create mod'' in
  repl ctx mod'' builder execution_engine' fenv venv cg_fenv

let _ =
  ignore(initialize ());
  let ctx = global_context () in
  let mod' = create_module ctx "Fact REPL" in
  let builder = Llvm.builder ctx in
  let execution_engine = create mod' in
  add_module mod' execution_engine;
  let fenv = Env.new_env () in
  let venv = Env.new_env () in
  let cg_fenv = Codegen.new_fenv () in
  repl ctx mod' builder execution_engine fenv venv cg_fenv
