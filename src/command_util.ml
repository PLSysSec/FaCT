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
  ignore(Llvm_X86.initialize());
  Lexer.file := Some f;
  let lexbuf = (try Lexing.from_channel (open_in f) with
    | _ -> raise (Exception "gvres")) in
  ignore(lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f });
  let ast = (try CModule (Parser.main Lexer.token lexbuf) with
      | _ -> let message = pos_string(to_pos ~buf:(Some lexbuf) lexbuf.lex_curr_p) in
        raise (SyntaxError ("Syntax error @ " ^ message))) in
  Format.printf "%s\n" (show_constantc_module ast);
  let _ =  tc_module ast in
  let core_ir = transform ast in
  let llvm_ctx = Llvm.create_context () in
  let llvm_mod = Llvm.create_module llvm_ctx "Module" in
  let _ = codegen llvm_ctx llvm_mod core_ir in
  let triple = Llvm_target.Target.default_triple () in
  let lltarget = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly = Llvm_target.TargetMachine.data_layout llmachine in
  Llvm.set_target_triple (Llvm_target.TargetMachine.triple llmachine) llvm_mod;
  Llvm.set_data_layout (Llvm_target.DataLayout.as_string lldly) llvm_mod;
  Llvm_analysis.assert_valid_module llvm_mod |> ignore;
  Llvm.print_module "out.ll" llvm_mod;
  Llvm_bitwriter.write_bitcode_file llvm_mod "meh.bc" |> ignore;
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
