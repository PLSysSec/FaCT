open Command_util
open Core.Std

let summary = "Compile the given const file."
let readme = "Compile a const file. Pass the relative path to the file " ^
             "as the first argument."
let o_doc = "Output Output object file name. Default is the input_file_name.o."
let llvm_doc = " Output LLVM to file"
let ast_doc = " Output AST to file"
let core_ir_doc = " Output Core IR to file"
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

let runner prep llvm_out ast_out core_ir_out =
  try compile prep llvm_out ast_out core_ir_out with
    | (Command_util.SyntaxError s) -> Log.error "%s" s
    | (Typecheck.FunctionNotDefined s) -> Log.error "%s" s

let compile_command =
  Command.basic
    ~summary:summary
    ~readme:(fun () -> readme)
    Command.Spec.(
      empty +>
      flag "-o" (optional string) ~doc:o_doc +>
      flag "-llvm-out" no_arg ~doc:llvm_doc +>
      flag "-ast-out" no_arg ~doc:ast_doc +>
      flag "-core-ir-out" no_arg ~doc:core_ir_doc +>
      flag "-debug" no_arg ~doc:debug_doc +>
      anon ("filename" %: string))
    (fun out_file llvm_out ast_out core_ir_out debug in_file ->
      set_log_level debug;
      let prep = prepare_compile out_file in_file () in
      runner prep llvm_out ast_out core_ir_out;
      (fun () -> ()))

let () = try
  Command.run ~version:"0.1" ~build_info:"Constantc Compiler" compile_command
    with | (Command_util.SyntaxError s) -> print_string ("Syntax Error:\n" ^ s)