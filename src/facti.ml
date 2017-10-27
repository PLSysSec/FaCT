open Llvm
open Llvm_executionengine
open Llvm_target
open Typecheck
open Pos
open Ctypes

(*
  This is the FaCT REPL. It is executed by building FaCT and executing
  `./facti.byte`. Right now, the REPL supports expressions and function
  declarations. In order to execute an expression, start the line with `:e` and
  end with `;;` This was to avoid parsing mishaps, but will be fixed in the
  future. To declare a function, start with `:f`.
  Here is an example of how to declare a function and call it.

  fact>
  fact> :f public int32 five() { return 5; }
  fact>
  fact> :e five() + 10;;
  15
  fact>

  Side note, function args are broken.

*)

exception REPL_Error

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

let texpr_to_fun (expr :Tast.top_level) (venv : (Tast.var_name * Tast.variable_type) Env.env) = match expr with
  | Tast.Expression expr ->
    begin
    match expr.data with
      | (expr',ty) ->
        let ret_ty = make_ast fake_pos ty in
        let name = "fun_ptr" ^ (string_of_int !count) in
        let fun_name = make_ast fake_pos name in
        count := !count + 1;
        let body = make_ast fake_pos (Tast.Return expr) in
        let block = (venv, [body]) in
        name, make_ast fake_pos (Tast.FunDec(fun_name, (Some ret_ty), [], block))
    end
  | Tast.Statement s -> print_string "vbareiugblaosr"; raise REPL_Error
  | _ -> raise REPL_Error


(*
  Used in the typecheck phase. We need to keep track of variables and functions
  declared in the REPL so they can be called. Scoping gets interesting here since we
  are allowed to declare variables outside of functions. To address this, free variables
  are not allowed in functions.
*)
type type_envs = {
  type_venv: (Tast.var_name * Tast.variable_type) Env.env;
  type_fenv: Tast.function_dec Env.env;
  type_arrenv: Tast.array_type Env.env;
}

(*
  Used in the codegen phase. We need to remember declared variables and functions.
*)
type llvm_envs = {
  llvm_venv: Llvm.llvalue Env.env;
  llvm_fenv: Codegen.fenv
}

let create_llvm_mod llcontext jit =
  let m = "Fact REPL " ^ (string_of_int !count) in
  let mod' = create_module llcontext m in
  add_module mod' jit;
  mod'

let jit_tast type_envs ll_envs ctx mod' builder jit cg_fenv = function
  | Tast.Expression expr as tast ->
    (* We need to transform the expr into a function so it can be JITed *)
    let name,tast_fun = texpr_to_fun tast type_envs.type_venv in
    print_string ((Tast.show_function_dec tast_fun) ^ "\n");
    Codegen.declare_prototypes ctx mod' builder cg_fenv expr.data;
    (* Generate LLVM *)
    Codegen.codegen_fun ctx.llcontext mod' builder cg_fenv false tast_fun |> ignore;
    (* JIT the function, returning a function pointer. *)
    run_static_ctors jit;
    let ty = Foreign.funptr (void @->returning int32_t) in
    let fun_ptr = get_function_address name ty jit in
    print_string "Evaluated to:\n";
    print_string (Int32.to_string (fun_ptr ()));
    print_newline ();
  | Tast.Statement st ->
    print_string ((Tast.show_statement st) ^ "\n");
    let cg_ctx =
      Codegen.mk_ctx ctx.llcontext mod' builder
      ll_envs.llvm_venv ll_envs.llvm_fenv type_envs.type_arrenv
      false in
    let block = type_envs.type_venv, [st] in
    Codegen.allocate_stack cg_ctx block;
    Codegen.codegen_stm cg_ctx None st |> ignore
  | Tast.FunctionDec fd  ->
    print_string ((Tast.show_function_dec fd) ^ "\n");
    Codegen.codegen_fun ctx.llcontext mod' builder cg_fenv false fd |> ignore

let rec repl2 mod' ctx builder jit type_envs ll_envs cg_fenv =
  (* Set the prompt *)
  print_string "fact>"; flush stdout;
  (* Lex and parse some stuff from stdin *)
  let ast = parse () in
  begin
    match ast with
    | None -> print_string "Parse error\n"
    | Some ast ->
      (* We made it to the typecheck phase *)
      let tast = tc_top_level type_envs ast in
      (* Now lets JIT according to the type *)
      jit_tast type_envs ll_envs ctx mod' builder jit cg_fenv tast
  end;
  let mod'' = create_llvm_mod ctx.llcontext jit in
  repl2 mod'' ctx builder jit type_envs ll_envs cg_fenv

and parse () =
  let print_syntax_error = function
    | Err.TypeError s -> print_string(s ^ "\n")
    | _ -> () in
  let lexbuf = Lexing.from_channel stdin in
  try Some(Parser.top_level Lexer.token lexbuf) with
    | _ -> let p = to_pos ~buf:(Some lexbuf) lexbuf.lex_curr_p lexbuf.lex_curr_p in
           print_syntax_error (Err.errSyntax p); None

and tc_top_level type_envs = function
  | Ast.FunctionDec fd ->
    let tast = Typecheck.tc_fdec type_envs.type_fenv fd in
    Tast.FunctionDec tast
  | Ast.Statement s ->
    let tc_ctx = Typecheck.{ rp=ref Tast.Public; pc=Tast.Public; venv=type_envs.type_venv; fenv=type_envs.type_fenv } in
    let tast = Typecheck.tc_stm tc_ctx s in
    Tast.Statement tast
  | Ast.Expression e ->
    let tc_ctx = Typecheck.{ rp=ref Tast.Public; pc=Tast.Public; venv=type_envs.type_venv; fenv=type_envs.type_fenv } in
    let tast = Typecheck.tc_expr tc_ctx e in
    Tast.Expression tast

let _ =
  ignore(initialize ());
  let llcontext = global_context () in
  let mod' = create_module llcontext "Initial module" in
  let builder = Llvm.builder llcontext in
  let execution_engine = create mod' in
  add_module mod' execution_engine;
  let fenv = Env.new_env () in
  let venv = Env.new_env () in
  let arrenv = Env.new_env () in
  let cg_fenv = Codegen.new_fenv () in
  let ll_venv = Env.new_env () in
  let type_envs = { type_fenv=fenv; type_venv=venv; type_arrenv=arrenv } in
  let ll_envs = { llvm_venv=ll_venv; llvm_fenv=cg_fenv } in
  (*let cg_ctx = { llcontext; llmodule; builder; venv; fenv; tenv; renv; verify_llvm }
  repl2 mod' cg_ctx builder execution_engine type_envs ll_envs cg_fenv
  *)()
