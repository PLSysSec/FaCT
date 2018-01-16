open Command_util
open Core
open Debugfun
open Opt

let summary = "Compile the given const file."
let readme = "Compile a const file. Pass the relative path to the file " ^
             "as the first argument."
let o_doc = "Output Output object file name. Default is the input_file_name.o."
let debug_doc = " Debug"
let ast_doc = " Output AST to file"
let core_ir_doc = " Output Core IR to file"
let pseudo_doc = " Output transformed pseudocode to file"
let llvm_doc = " Output LLVM to file"
let header_doc = " Output C header to file"
let verify_llvm_doc = "Verify LLVM IR with ct-verif"
let mode_doc = "mode The mode to compile with (dev or prod)"
let opt_level_doc = "level The level of optimization to run (O0, 01, 02, or OF)"
let verify_opt_doc = "opt Comma separated list of optimzations to verify on the FaCT program. They are run in the order in which they are provided"

let normalize_out_file out_file =
  Filename.chop_extension(Filename.basename out_file)

(* Prepares and normalizes the input/output files.
   Returns a tuple where the first is the input, second is the output name,
   third is the output directory *)
let prepare_compile out_file (in_files : string list) () =
  Log.debug "Preparing to compile";
  let base = Filename.chop_extension(Filename.basename (List.last_exn in_files)) in
  (match out_file with
    | None -> (in_files, base, Filename.dirname (List.last_exn in_files))
    | Some f -> (in_files, normalize_out_file f, Filename.dirname f))

let set_log_level debug =
  Log.set_output stdout;
  Log.color_on ();
  match debug with
    | true -> Log.set_log_level Log.DEBUG
    | false -> Log.set_log_level Log.ERROR

let error_exit s =
  let ss = Str.bounded_split (Str.regexp_string " ") s 3 in
    ANSITerminal.eprintf [ANSITerminal.white] "%s "  (List.nth_exn ss 0);
    ANSITerminal.eprintf [ANSITerminal.red]   "%s "  (List.nth_exn ss 1);
    Printf.eprintf                            "%s\n" (List.nth_exn ss 2);
    exit 1

let runner prep args =
  (*compile prep args*)
  try compile prep args with
    | (Err.InternalCompilerError s) ->
      let backtrace = Printexc.get_backtrace () in
      let lines = Str.split (Str.regexp_string "\n") backtrace in
      let rlines = List.rev lines in
        List.iter rlines print_endline;
        error_exit s(*
    | (Err.VariableNotDefined s) -> error_exit s
    | (Err.LabelError s) -> error_exit s
    | (Err.UnclassifiedError s) -> error_exit s
    | (Err.TypeError s) -> error_exit s
    | (Err.SMTSolverError s) -> error_exit ("error: "^s)
    | (Err.NotImplemented s) -> error_exit ("error: "^s)*)
    (*| (Command_util.SyntaxError s) -> error_exit s
    | (Codegen.Error s) -> error_exit s
    | (Command_util.SyntaxError s) -> error_exit s
    | (Codegen.Error s) -> error_exit s
    | (Typecheck.NotImplemented) -> error_exit "Not implemented"
    | (Env.VariableNotDefined s) -> error_exit s
    | (Env.FunctionNotDefined s) -> error_exit s
    | (Typecheck.TypeError s) -> error_exit s
    | (Typecheck.UnknownType s) -> error_exit s
    | (Typecheck.CallError s) -> error_exit s
    | (Typecheck.ForError s) -> error_exit s
    | _ -> error_exit "Error"*)

let compile_command =
  Command.basic
    ~summary:summary
    ~readme:(fun () -> readme)
    Command.Spec.(
      empty +>
      flag "-o" (optional string) ~doc:o_doc +>
      flag "-debug" no_arg ~doc:debug_doc +>
      flag "-ast-out" no_arg ~doc:ast_doc +>
      flag "-core-ir-out" no_arg ~doc:core_ir_doc +>
      flag "-pseudocode" no_arg ~doc:pseudo_doc +>
      flag "-llvm-out" no_arg ~doc:llvm_doc +>
      flag "-generate-header" no_arg ~doc:header_doc +>
      flag "-verify-llvm" no_arg ~doc:verify_llvm_doc +>
      flag "-mode" (optional string) ~doc:mode_doc +>
      flag "-opt" (optional string) ~doc:opt_level_doc +>
      flag "-verify-opt" (optional string) ~doc:verify_opt_doc +>
      anon (sequence ("filename" %: file)))
    (fun
      out_file
      debug
      ast_out
      core_ir_out
      pseudo_out
      llvm_out
      gen_header
      verify_llvm
      mode
      opt_level
      verify_opts
      in_files () ->
      let mode = match mode with
        | Some "dev" -> DEV
        | Some "prod" -> PROD
        | Some m -> error_exit ("Unknown mode: " ^ m ^". Expected dev or prod")
        | None -> PROD in
      let opt_level = match opt_level with
        | Some "O0" -> O0
        | Some "O1" -> O1
        | Some "O2" -> O2
        | Some "OF" -> OF
        | Some o -> error_exit ("Unknown optimization level: " ^ o ^ ". Expected O0, O1, O2, or OF")
        | None -> O0 in
      let args = { in_files; out_file; debug;
                   ast_out; core_ir_out; pseudo_out;
                   llvm_out; gen_header; verify_llvm; mode; opt_level; 
                   verify_opts } in
        set_log_level debug;
        let prep = prepare_compile out_file in_files () in
          runner prep args)

let () =
  Command.run ~version:"0.1" ~build_info:"FaCT Compiler" compile_command
