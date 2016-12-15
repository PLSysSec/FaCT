open Cast
open Codegen
open Ast
open Env
open Typecheck
open Transform
open Lexing

exception Exception of string
exception SyntaxError of string

let run_command c args =
  let a  = Unix.fork () in
  match a with
  | 0 -> (try
            Unix.execvp c args
          with
            _ -> Printf.printf "%s" "error while execv\n"; exit (-1))
  | -1 -> Printf.printf "%s" "error accured on fork\n"
  | _ -> ignore (Unix.wait ())

let compile f =
  Lexer.file := Some f;
  let lexbuf = (try Lexing.from_channel (open_in f) with
    | _ -> raise (Exception "gvres")) in
  ignore(lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f });
  let ast = (try CModule (Parser.main Lexer.token lexbuf) with
      | _ -> let message = pos_string(to_pos ~buf:(Some lexbuf) lexbuf.lex_curr_p) in
           raise (SyntaxError ("Syntax error @ " ^ message))) in
  let _ =  tc_module ast in
  let core_ir = transform ast in
  let llvm_ctx = Llvm.create_context () in
  let llvm_mod = Llvm.create_module llvm_ctx "Module" in
  let _ = codegen llvm_ctx llvm_mod core_ir in
  Llvm.print_module "out.ll" llvm_mod;
  ()

let run = (fun () -> run_command "lli" [|"lli"; "out.ll"|])
let link = (fun () -> run_command "llvm-as" [|"llvm-as"; "out.ll"|])
let assemble = (fun () -> run_command "llc" [|"llc"; "out.bc"|])
let share = (fun () -> run_command "clang" [|"clang"; "-c"; "out.s"|])
let compile_harness = (fun () -> run_command "gcc" [|"gcc"; "-c"; "harness.c"|])
let compile_ssl = (fun () -> run_command "gcc" [|"gcc"; "-c"; "ssl.c"|])
let compile_c cf =
  (fun () -> run_command "gcc" [|"gcc"; "Unity/src/unity.c"; "-o"; "final"; cf; "out.o"|])
let clean = (fun () ->
    run_command "rm"
      [|"rm"; "out.ll"; "out.bc"; "out.o"; "out.s"; "final"; "harness.o"|])
