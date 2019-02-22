open Lwt
open Pos
open Err
open Lexing

type opt_level = O0 | O1 | O2 | O3 | OF

type args_record = {
  in_files    : string list;
  out_file    : string option;
  debug       : bool;
  ast_out     : bool;
  pseudo_out  : bool;
  llvm_out    : bool;
  gen_header  : bool;
  verify_llvm : bool;
  opt_level   : opt_level;
  shared      : bool;
  no_inline_asm : bool;
  addl_opts   : string list;
}

let run_command c args exit_on_error =
  let process = Lwt_process.exec (c,args) in
  let handler = function
    | Unix.WEXITED s ->
      Log.debug "Command exited. Code %d" s;
      Lwt.return s
    | Unix.WSIGNALED s ->
      Log.debug "Command signaled to stop. Code %d" s;
      Lwt.return s
    | Unix.WSTOPPED s ->
      Log.debug "Error occured on command. Code %d" s;
      Lwt.return s in
  let ret_code = Lwt_main.run (process >>= handler) in
  match exit_on_error with
    | true when ret_code != 0 ->
      raise @@ InternalCompilerError("error: " ^ (String.concat " " (Array.to_list args)))
    | _ -> ret_code

let generate_out_file out_dir out_file = out_dir ^ "/" ^ out_file

let output_ast ast_out out_file ast =
  if ast_out then
    let ast_out_file = out_file ^ ".ast.ml" in
      Log.debug "Outputting AST to %s" ast_out_file;
      Core.Out_channel.write_all ast_out_file
        ~data:((Ast.show_fact_module ast)^"\n")

let output_tast ast_out out_file tast =
  if ast_out then
    let tast_out_file = out_file ^ ".tast.ml" in
      Log.debug "Outputting TAST to %s" tast_out_file;
      Core.Out_channel.write_all tast_out_file
        ~data:((Tast.show_fact_module tast)^"\n")

let generate_pseudo gen_pseudo out_file tast =
  if gen_pseudo then
    let pseudo_out_file = out_file ^ ".pseudo.fact" in
      Log.debug "Outputting pseudocode file to %s" pseudo_out_file;
      Core_kernel.Out_channel.write_all pseudo_out_file
        ~data:(Pseudocode.transform tast)

let output_llvm llvm_out out_file llvm_mod =
  if llvm_out then
    let out_file' = out_file ^ ".ll" in
      Log.debug "Outputting LLVM IR to %s" out_file';
      Llvm.print_module out_file' llvm_mod

let generate_header gen_header out_file xftast =
  if gen_header then
    let header_out_file = out_file ^ ".h" in
      Log.debug "Outputting header file to %s" header_out_file;
      Core_kernel.Out_channel.write_all header_out_file
        ~data:(Header.generate_header header_out_file xftast)

let output_bitcode out_file llvm_mod =
  let out_file' = out_file ^ ".bc" in
    Log.debug "Outputting LLVM bitcode to %s" out_file';
    match Llvm_bitwriter.write_bitcode_file llvm_mod out_file' with
      | false -> Log.error "An error occurred printing LLVM bitcode"; exit 1
      | true -> Log.debug "Successfully output LLVM bitcode"

let output_assembly args out_file =
  let out_file_bc = out_file ^ ".bc" in
  let out_file_s = out_file ^ ".s" in
  let out_file_ll = out_file ^ ".ll" in
    Log.debug "Creating .s file at %s" out_file_s;
    let addl_opts =
      List.flatten @@
      List.map
        (fun opts -> Str.split (Str.regexp " +") opts)
        args.addl_opts in
    let opt_arg =
      match args.opt_level with
        | O0 -> "-O0"
        | O1 -> "-O1"
        | O2 -> "-O2"
        | O3 -> "-O3"
        | OF -> "-OF" in
    let clang_args =
      [ "clang-6.0"; "-S"; opt_arg;
        out_file_bc; ] @ addl_opts in
      if args.llvm_out then
        run_command
          "clang-6.0"
          (Array.of_list
             (clang_args @ ["-emit-llvm"; "-o"; out_file_ll]))
          true |> ignore;
      run_command
        "clang-6.0"
        (Array.of_list
           (clang_args @ ["-o"; out_file_s]))
        true |> ignore

let output_object args out_file =
  let out_file_s = out_file ^ ".s" in
  let out_file_o = out_file ^ ".o" in
    Log.debug "Creating object file at %s" out_file_o;
    let addl_opts =
      List.flatten @@
      List.map
        (fun opts -> Str.split (Str.regexp " +") opts)
        args.addl_opts in
    let opt_arg =
      match args.opt_level with
        | O0 -> "-O0"
        | O1 -> "-O1"
        | O2 -> "-O2"
        | O3 -> "-O3"
        | OF -> "-OF" in
    let clang_args =
      [ "clang-6.0"; "-c"; opt_arg;
        out_file_s; ]
      @ addl_opts
      @ [ "-o"; out_file_o ] in
      run_command
        "clang-6.0"
        (Array.of_list clang_args)
        true |> ignore

let output_shared_object out_file args =
  if not args.shared then () else
    let out_file_s = out_file ^ ".s" in
    let out_file_o = out_file ^ ".so" in
      Log.debug "Creating shared object file at %s" out_file_o;
      let addl_opts =
        List.flatten @@
        List.map
          (fun opts -> Str.split (Str.regexp " +") opts)
          args.addl_opts in
      let opt_arg =
        match args.opt_level with
          | O0 -> "-O0"
          | O1 -> "-O1"
          | O2 -> "-O2"
          | O3 -> "-O3"
          | OF -> "-OF" in
      let clang_args =
        [ "clang-6.0"; "-shared"; opt_arg;
          out_file_s; ]
        @ addl_opts
        @ [ "-o"; out_file_o ] in
        run_command
          "clang-6.0"
          (Array.of_list clang_args)
          true |> ignore

let ctverify verif out_file tast =
  if verif then
    let wrapper_out_file = out_file ^ "_wrapper.c" in
    Log.debug "Outputting ctverif wrapper file to %s" wrapper_out_file;
    Core_kernel.Out_channel.write_all wrapper_out_file ~data:(Ctverif.ctverify out_file tast);
    Log.debug "To verify, run: `verif.sh [ENTRYPOINT] %s %s %s`" wrapper_out_file (out_file^".ll") (out_file^".h")

let compile (in_files,out_file,out_dir) args =
  let out_file' = generate_out_file out_dir out_file in
  let lex_and_parse in_file =
    Log.debug "Compiling %s" in_file;
    Lexer.file := Some in_file;
    let lexbuf =
      (try Lexing.from_channel (open_in in_file) with
        | _ -> raise (InternalCompilerError "Lexing failed"))
    in
      ignore(lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = in_file });
      Log.debug "Lexing complete";
      let ast =
        (try (Parser.main Lexer.token lexbuf) with
          | _ -> let p = to_pos ~buf:(Some lexbuf) lexbuf.lex_curr_p lexbuf.lex_curr_p in
              raise (errSyntax p))
      in
        Log.debug "Parsing complete";
        ast
  in
  let asts = List.map lex_and_parse in_files in
  let all_fdecs = List.fold_left (fun fdecs (Ast.Module (more_fdecs,_)) -> fdecs @ more_fdecs) [] asts in
  let all_sdecs = List.fold_left (fun sdecs (Ast.Module (_,more_sdecs)) -> sdecs @ more_sdecs) [] asts in
  let ast = Ast.Module (all_fdecs,all_sdecs) in (* all files combined *)
    output_ast args.ast_out out_file' ast;
  let ast = Cyclecheck.transform ast in (* fns sorted topologically, callers first *)
    output_ast args.ast_out out_file' ast;
  let ast = Constfold.transform ast in (* constant folding *)
    output_ast args.ast_out out_file' ast;
  let ast = Varrename.transform ast in (* unique named vars *)
    output_ast args.ast_out out_file' ast;
  let ast = Fnextract.transform ast in (* function calls are no longer expressions *)
    output_ast args.ast_out out_file' ast;
  let ast = Arr_speccer.transform ast in (* array lengths filled in *)
    output_ast args.ast_out out_file' ast;
  let tast = Typecheck.transform ast in (* transition to tast; exprs have types *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  let tast = Sanitycheck.transform false tast in (* check that everything is correct before transforms *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  let tast = Oobcheck.transform args.debug tast in (* array accesses etc. validated *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  let llctx,llmod = Codegen.codegen args.no_inline_asm tast in
    output_llvm args.llvm_out out_file' llmod;
  let tast = Transfn.transform tast in (* transform secret fn calls *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  let tast = Transret.transform tast in (* transform secret early returns *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  let tast = Transbranch.transform tast in (* transform secret branchs *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  let tast = Sanitycheck.transform true tast in (* check that everything is correct after transforms *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
    generate_header (args.gen_header || args.verify_llvm) out_file' tast;
  let llctx,llmod = Codegen.codegen args.no_inline_asm tast in
    output_bitcode out_file' llmod;
    output_assembly args out_file' |> ignore;
    output_shared_object out_file' args;
    output_object args out_file' |> ignore;
  ctverify args.verify_llvm out_file' tast
