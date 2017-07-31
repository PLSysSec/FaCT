open Llvm
open Llvm_executionengine
open Llvm_target
open Typecheck
open Pos

let fake_pos = { file="JIT"; line=0; lpos=0; rpos=0 }

let expr_to_fun e =
  let ret_int = make_ast fake_pos (Ast.Int 32) in
  let pub = make_ast fake_pos Ast.Public in
  let ret_type = make_ast fake_pos (Ast.BaseET(ret_int, pub)) in 
  let fun_name = make_ast fake_pos "" in
  let body = make_ast fake_pos (Ast.Return e) in
  make_ast fake_pos (Ast.FunDec(fun_name, (Some ret_type), [], [body]))

let rec repl ctx mod' builder execution_engine fenv venv =
  print_string "fact>"; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.expr_top Lexer.token lexbuf in
  print_string ((Ast.show_expr ast) ^ "\n");
  let ast' = expr_to_fun ast in
  let tast = tc_fdec fenv ast' in
  print_string ((Tast.show_function_dec tast) ^ "\n");
  let f = Codegen.codegen_fun ctx mod' builder tast in
  (* JIT the function, returning a function pointer. *)

  print_string "Evaluated to ";
  (*print_float (GenericValue.as_float Codegen.double_type result);*)
  print_newline ();
  repl ctx mod' builder execution_engine fenv venv

let _ =
  ignore(initialize ());
  let ctx = global_context () in
  let mod' = create_module ctx "Fact REPL" in
  let builder = Llvm.builder ctx in
  let execution_engine = create mod' in
  let fenv = Env.new_env () in
  let venv = Env.new_env () in
  repl ctx mod' builder execution_engine fenv venv