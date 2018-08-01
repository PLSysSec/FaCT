open Command_util
(*open Debugfun
open Opt*)

let summary = "Compile the given const file."
let readme = "Compile a const file. Pass the relative path to the file " ^
             "as the first argument."
let o_doc = "Output Output object file name. Default is the input_file_name.o."
let debug_doc = " Debug"
let ast_doc = " Output AST to file"
let core_ir_doc = " Output Core IR to file"
let pseudo_doc = " Output transformed pseudocode to file"
let smack_doc = " Output LLVM for Smack verification to file"
let llvm_doc = " Output LLVM to file"
let header_doc = " Output C header to file"
let verify_llvm_doc = "Verify LLVM IR with ct-verif"
let mode_doc = "mode The mode to compile with (dev or prod)"
let opt_level_doc = "level The level of optimization to run (O0, 01, 02, or OF)"
let opt_limit_doc = "seconds The number of seconds to run the optimizer at OF before quitting and picking the best pipeline"
let verify_opt_doc = "opt Comma separated list of optimzations to verify on the FaCT program. They are run in the order in which they are provided"
let shared_opt_doc = "Generate a .so file"
let noguac_opt_doc = "Don't run the name-based LLVM IR verifier"

let normalize_out_file out_file =
  Filename.chop_extension(Filename.basename out_file)

(* Prepares and normalizes the input/output files.
   Returns a tuple where the first is the input, second is the output name,
   third is the output directory *)
let prepare_compile out_file (in_files : string list) () =
  Log.debug "Preparing to compile";
  let base = Filename.chop_extension(Filename.basename (Core.List.last_exn in_files)) in
  (match out_file with
    | None -> (in_files, base, Filename.dirname (Core.List.last_exn in_files))
    | Some f -> (in_files, normalize_out_file f, Filename.dirname f))

let set_log_level debug =
  Log.set_output stderr;
  Log.color_on ();
  match debug with
    | true -> Log.set_log_level Log.DEBUG
    | false -> Log.set_log_level Log.ERROR

let error_exit s =
  let ss = Str.bounded_split (Str.regexp_string " ") s 3 in
    ANSITerminal.eprintf [ANSITerminal.white] "%s "  (List.nth ss 0);
    ANSITerminal.eprintf [ANSITerminal.red]   "%s "  (List.nth ss 1);
    Printf.eprintf                            "%s\n" (List.nth ss 2);
    exit 1

let runner prep args =
  try compile prep args with
    | _ as exn ->
      begin
        match args.debug with
          | false -> ()
          | true  ->
            let backtrace = Printexc.get_backtrace () in
            let lines = Str.split (Str.regexp_string "\n") backtrace in
            let rlines = List.rev lines in
              List.iter (fun s -> Printf.eprintf "%s\n" s) rlines
      end;
      begin match exn with
        | (Err.VariableNotDefined s)
        | (Err.InternalCompilerError s) ->
          error_exit s
        | _ as e ->
          Printf.eprintf "%s\n" (Printexc.to_string e)
      end

let test_graph () =
  (*let g = Graphf.create_graph () in
  let v1 = Graphf.create_vertex Optf.AggressiveDCE in
  let v2 = Graphf.create_vertex Optf.AlignmentFromAssumptions in
  let v3 = Graphf.create_vertex Optf.AlignmentFromAssumptions  in
  let v4 = Graphf.create_vertex Optf.AlwaysInliner in
  let v5 = Graphf.create_vertex Optf.AlwaysInliner in
  let v6 = Graphf.create_vertex Optf.BasicAliasAnalysis in
  let v7 = Graphf.create_vertex Optf.ArgumentPromotion in
  let v8 = Graphf.create_vertex Optf.ConstantMerge in
  Graphf.add_edge g v1 v2|> ignore;
  Graphf.add_edge g v2 v4|> ignore;
  Graphf.add_edge g v1 v5|> ignore;
  Graphf.add_edge g v5 v6|> ignore;
  Graphf.add_edge g v6 v7|> ignore;
  Graphf.add_edge g v2 v8|> ignore;
  (*Graphf.add_edge g v2 v5|> ignore;*)
  Graphf.dump_graph g;

  Graphf.traverse g v1;
  let pipeline = Graphf.get_pipelines g [] v1 in

  let print_pipeline p =
    let p' = List.map (fun v -> Optf.show_optimization(Graphf.vertex_opt(v))) p in
    let s = String.concat " -> " p' in
    Log.error "Pipeline: %s" s; in

  List.iter print_pipeline pipeline;

  Pipeline.drive ();*)

  (*Graphf.draw_graph (ref g);*)
  error_exit "Testing graph"

let compile_command =
  Core.Command.basic_spec
    ~summary:summary
    ~readme:(fun () -> readme)
    Core.Command.Spec.(
      empty +>
      flag "-o" (optional string) ~doc:o_doc +>
      flag "-debug" no_arg ~doc:debug_doc +>
      flag "-ast-out" no_arg ~doc:ast_doc +>
      flag "-core-ir-out" no_arg ~doc:core_ir_doc +>
      flag "-pseudocode" no_arg ~doc:pseudo_doc +>
      flag "-smack-out" no_arg ~doc:smack_doc +>
      flag "-llvm-out" no_arg ~doc:llvm_doc +>
      flag "-generate-header" no_arg ~doc:header_doc +>
      flag "-verify-llvm" no_arg ~doc:verify_llvm_doc +>
      flag "-mode" (optional string) ~doc:mode_doc +>
      flag "-opt" (optional string) ~doc:opt_level_doc +>
      flag "-limit" (optional int) ~doc:opt_limit_doc +>
      flag "-verify-opt" (optional string) ~doc:verify_opt_doc +>
      flag "-shared" no_arg ~doc:shared_opt_doc +>
      flag "-no-guac" no_arg ~doc:noguac_opt_doc +>
      anon (sequence ("filename" %: file)))
    (fun
      out_file
      debug
      ast_out
      core_ir_out
      pseudo_out
      smack_out
      llvm_out
      gen_header
      verify_llvm
      mode
      opt_level
      opt_limit
      verify_opts
      shared
      noguac
      in_files () ->
      (*let mode = match mode with
        | Some "dev" -> DEV
        | Some "prod" -> PROD
        | Some m -> error_exit ("Unknown mode: " ^ m ^". Expected dev or prod")
        | None -> PROD in
      let opt_level = match opt_level with
        | Some "O0" -> O0
        | Some "O1" -> O1
        | Some "O2" -> O2
        | Some "O3" -> O3
        | Some "OF" -> OF
        | Some o -> error_exit ("Unknown optimization level: " ^ o ^ ". Expected O0, O1, O2, O3, or OF")
        | None -> O0 in*)
      let args = { in_files; out_file; debug;
                   ast_out; core_ir_out; pseudo_out; smack_out;
                   llvm_out; gen_header; verify_llvm; (*mode; opt_level;
                   opt_limit; verify_opts; shared; noguac*) } in
        set_log_level debug;
        let prep = prepare_compile out_file in_files () in
          runner prep args)

let () =
  Core.Command.run ~version:"0.1" ~build_info:"FaCT Compiler" compile_command
