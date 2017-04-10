open Pos
open Lexing
open Err
open Tast
open Cast
open Env
open Typecheck
(*
open Codegen


open Transform
*)

let run_command c args =
  let a  = Unix.fork () in
  match a with
  | 0 -> (try
            Unix.execvp c args
          with
            _ -> Printf.printf "%s" "error while execv\n"; exit (-1))
  | -1 -> Printf.printf "%s" "error accured on fork\n"
  | _ -> ignore (Unix.wait ())

let output_ast ast_out out_file ast =
  match ast_out with
    | false -> Log.debug "Not outputting AST"
    | true ->
      let ast_out_file = out_file ^ ".ast.ml" in
      Log.debug "Outputting AST to %s" ast_out_file;
      Core.Std.Out_channel.write_all ast_out_file
        ~data:(Ast.show_fact_module ast)

let output_tast ast_out out_file tast =
  match ast_out with
    | false -> Log.debug "Not outputting TAST"
    | true ->
      let tast_out_file = out_file ^ ".tast.ml" in
        Log.debug "Outputting TAST to %s" tast_out_file;
        Core.Std.Out_channel.write_all tast_out_file
          ~data:(Tast.show_fact_module tast)

let output_core_ir core_ir_out out_file core_ir =
  match core_ir_out with
    | false -> Log.debug "Not outputting core IR"
    | true ->
      let core_ir_out_file = out_file ^ ".core.ml" in
      Log.debug "Outputting core IR to %s" core_ir_out_file;
      Core.Std.Out_channel.write_all core_ir_out_file
        ~data:(show_cmodule core_ir)

let generate_out_file out_dir out_file = out_dir ^ "/" ^ out_file

let output_llvm llvm_out out_file llvm_mod =
  match llvm_out with
    | false -> Log.debug "Not outputting LLVM IR"
    | true ->
      let out_file' = out_file ^ ".ll" in
      Log.debug "Outputting LLVM IR to %s" out_file';
      Llvm.print_module out_file' llvm_mod

let output_bitcode out_file llvm_mod =
  let out_file' = out_file ^ ".bc" in
  Log.debug "Outputting LLVM bitcode to %s" out_file';
  match Llvm_bitwriter.write_bitcode_file llvm_mod out_file' with
    | false -> Log.error "An error occurred printing LLVM bitcode"; exit (-1)
    | true -> Log.debug "Successfully output LLVM bitcode"

let output_shared out_file =
  let out_file' = out_file ^ ".bc" in
  let out_file_s = out_file ^ ".s" in
  Log.debug "Createing .s file at %s" out_file_s;
  run_command "llc" [|"llc"; out_file'|]

let output_object out_file =
  let out_file_s = out_file ^ ".s" in
  let out_file_o = out_file ^ ".o" in
  Log.debug "Creating object file at %s" out_file_o;
  run_command "clang" [|"clang"; "-c"; out_file_s|]

let compile (in_file,out_file,out_dir) llvm_out ast_out core_ir_out =
  let out_file' = generate_out_file out_dir out_file in
  Log.debug "Compiling %s" in_file; 
  ignore(Llvm_X86.initialize());
  Lexer.file := Some in_file;
  let lexbuf = (try Lexing.from_channel (open_in in_file) with
    | _ -> raise_error_np LexingError) in
  ignore(lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = in_file });
  Log.debug "Lexing complete";
  let ast = Ast.Module (Parser.main Lexer.token lexbuf) in
  (*let ast = (try CModule (Parser.main Lexer.token lexbuf) with
      | _ -> raise_error
              (to_pos ~buf:(Some lexbuf) lexbuf.lex_curr_p) SyntaxError) in*)
  Log.debug "Parsing complete";
  output_ast ast_out out_file' ast;
  let tast = tc_module ast in ()
  (*output_tast ast_out out_file' tast;
  Log.debug "Typecheck complete";
  let core_ir = transform tast in
  Log.debug "Core IR transform complete";
  output_core_ir core_ir_out out_file' core_ir;
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
  output_llvm llvm_out out_file' llvm_mod;
  output_bitcode out_file' llvm_mod;
  output_shared out_file';
  output_object out_file'*)

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
