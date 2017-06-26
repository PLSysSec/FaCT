open Command_util
open Core

let summary = "Compile the given const file."
let readme = "Compile a const file. Pass the relative path to the file " ^
             "as the first argument."
let o_doc = "Output Output object file name. Default is the input_file_name.o."
let ast_doc = " Output AST to file"
let core_ir_doc = " Output Core IR to file"
let llvm_doc = " Output LLVM to file"
let debug_doc = " Debug"

let normalize_out_file out_file =
  Filename.chop_extension(Filename.basename out_file)

(* Prepares and normalizes the input/output files.
   Returns a tuple where the first is the input, second is the output name,
   third is the output directory *)
let prepare_compile out_file in_file () =
  Log.debug "Preparing to compile";
  let base = Filename.chop_extension(Filename.basename in_file) in
  (match out_file with
    | None -> (in_file, base, Filename.dirname in_file)
    | Some f -> (in_file, normalize_out_file f, Filename.dirname f))

let set_log_level debug =
  Log.set_output stdout;
  match debug with
    | true -> Log.set_log_level Log.DEBUG
    | false -> Log.set_log_level Log.ERROR

let error_exit s =
  Printf.eprintf "%s\n" s;
  exit 1

let runner prep ast_out core_ir_out llvm_out =
  try compile prep ast_out core_ir_out llvm_out with
    | (Err.InternalCompilerError s) -> error_exit s
    | (Err.VariableNotDefined s) -> error_exit s
    | (Err.LabelError s) -> error_exit s
    | (Err.UnclassifiedError s) -> error_exit s
    | (Err.SMTSolverError s) -> error_exit ("error: "^s)
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
      flag "-ast-out" no_arg ~doc:ast_doc +>
      flag "-core-ir-out" no_arg ~doc:core_ir_doc +>
      flag "-llvm-out" no_arg ~doc:llvm_doc +>
      flag "-debug" no_arg ~doc:debug_doc +>
      anon ("filename" %: string))
    (fun out_file ast_out core_ir_out llvm_out debug in_file () ->
      set_log_level debug;
      let prep = prepare_compile out_file in_file () in
      runner prep ast_out core_ir_out llvm_out)

let () =
  Command.run ~version:"0.1" ~build_info:"FaCT Compiler" compile_command
