open Lwt
open Pos
open Err
open Lexing

(*
open Typecheck
open Codegen
open Debugfun
open Opt
open Jank
open Sys
*)

type args_record = {
  in_files    : string list;
  out_file    : string option;
  debug       : bool;
  ast_out     : bool;
  core_ir_out : bool;
  pseudo_out  : bool;
  smack_out   : bool;
  llvm_out    : bool;
  gen_header  : bool;
  verify_llvm : bool;
(*  mode        : mode;
  opt_level   : opt_level;
  opt_limit   : seconds option;
  verify_opts : string option;
  shared      : bool;
  noguac      : bool;*)
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
  match ast_out with
    | false -> Log.debug "Not outputting AST"
    | true ->
      let ast_out_file = out_file ^ ".ast.ml" in
        Log.debug "Outputting AST to %s" ast_out_file;
        Core.Out_channel.write_all ast_out_file
          ~data:((Ast.show_fact_module ast)^"\n")

let output_tast ast_out out_file tast =
  match ast_out with
    | false -> Log.debug "Not outputting TAST"
    | true ->
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

(*
let output_xftast xftast_out out_file tast =
  match xftast_out with
    | false -> Log.debug "Not outputting transformed TAST"
    | true ->
      let tast_out_file = out_file ^ ".xftast.ml" in
        Log.debug "Outputting transformed TAST to %s" tast_out_file;
        Core_kernel.Out_channel.write_all tast_out_file
          ~data:((Tast.show_fact_module tast)^"\n")

let generate_header gen_header out_file xftast =
  if gen_header then
    let header_out_file = out_file ^ ".h" in
      Log.debug "Outputting header file to %s" header_out_file;
      Core_kernel.Out_channel.write_all header_out_file
        ~data:(Header.generate_header out_file xftast)

let output_llvm llvm_out out_file llvm_mod =
  match llvm_out with
    | false -> Log.debug "Not outputting LLVM IR"
    | true ->
      let out_file' = out_file ^ ".ll" in
      Log.debug "Outputting LLVM IR to %s" out_file';
      Llvm.print_module out_file' llvm_mod

let generate_smack args out_file xftast =
  let do_output out_file_name tast =
    generate_pseudo args.pseudo_out out_file_name tast;
    let llvm_ctx = Llvm.create_context () in
    let llvm_mod = Llvm.create_module llvm_ctx "SmackModule" in
    let llvm_builder = Llvm.builder llvm_ctx in
    let tast = Transform_args.xf_module tast in
      codegen llvm_ctx llvm_mod llvm_builder false tast;
      output_llvm true out_file_name llvm_mod;
      let lines = ref [] in
      let chan = open_in (out_file_name ^ ".ll") in
        begin
          try
            while true; do
              lines := input_line chan :: !lines
            done
          with End_of_file ->
            close_in chan
        end;
        lines := List.rev !lines;
        lines := "; verify with: smack --bit-precise --verifier=boogie --modular --entry-point=[...]" :: !lines;
        let outfile = open_out (out_file_name ^ ".ll") in
          output_string outfile @@ String.concat "\n" !lines;
          close_out outfile in
    if args.smack_out then
      begin
        Log.debug "Generating Smack output";
        let out_file' = out_file ^ ".smack" in
        let smacktast = Smack.transform xftast in
          do_output out_file' smacktast;
          (*let out_file' = out_file ^ ".smack.uninit" in
          let smacktast = Smack_uninit.transform xftast in
            do_output out_file' smacktast*)
      end


let output_bitcode out_file llvm_mod =
  let out_file' = out_file ^ ".bc" in
  Log.debug "Outputting LLVM bitcode to %s" out_file';
  match Llvm_bitwriter.write_bitcode_file llvm_mod out_file' with
    | false -> Log.error "An error occurred printing LLVM bitcode"; exit (-1)
    | true -> Log.debug "Successfully output LLVM bitcode"

let output_assembly opt_level out_file =
  let out_file' = out_file ^ ".bc" in
  let out_file_s = out_file ^ ".s" in
  let out_file_fpic_s = out_file ^ ".fpic.s" in
  Log.debug "Creating .s file at %s" out_file_s;
  match opt_level with
    | O2 ->
      begin
        run_command "llc-6.0" [|"llc-6.0"; "-O2"; "-mcpu=core-avx2"; out_file'|] true |> ignore;
        run_command "llc-6.0" [|"llc-6.0"; "-O2"; "-mcpu=core-avx2"; "-relocation-model=pic"; out_file'; "-o"; out_file_fpic_s|] true
      end
    | O3 ->
      begin
        run_command "llc-6.0" [|"llc-6.0"; "-O3"; "-mcpu=core-avx2"; out_file'|] true |> ignore;
        run_command "llc-6.0" [|"llc-6.0"; "-O3"; "-mcpu=core-avx2"; "-relocation-model=pic"; out_file'; "-o"; out_file_fpic_s|] true
      end
    | _ ->
      begin
        run_command "llc-6.0" [|"llc-6.0"; "-mcpu=core-avx2"; out_file'|] true |> ignore;
        run_command "llc-6.0" [|"llc-6.0"; "-mcpu=core-avx2"; "-relocation-model=pic"; out_file'; "-o"; out_file_fpic_s|] true
      end

let output_object out_file =
  let out_file_s = out_file ^ ".s" in
  let out_file_fpic_s = out_file ^ ".fpic.s" in
  let out_file_o = out_file ^ ".o" in
  let out_file_fpic = out_file ^ ".fpic.o" in
  Log.debug "Creating object file at %s" out_file_o;
  run_command "clang-6.0" [|"clang-6.0"; "-c"; out_file_s; "-o"; out_file_o|] true |> ignore;
  run_command "clang-6.0" [|"clang-6.0"; "-c"; out_file_fpic_s; "-o"; out_file_fpic|] true

let output_shared_object out_file args =
  if not args.shared then () else
  let out_file_s = out_file ^ ".s" in
  let out_file_fpic_s = out_file ^ ".fpic.s" in
  let out_file_o = out_file ^ ".so" in
  let out_file_fpic = out_file ^ ".fpic.so" in
  Log.debug "Creating object file at %s" out_file_o;
  run_command "clang-6.0"
    [|"clang-6.0"; "-shared"; out_file_s; "-o"; out_file_o|] true |> ignore;
  run_command "clang-6.0"
    [|"clang-6.0"; "-shared"; out_file_fpic_s; "-o"; out_file_fpic|] true |> ignore

let verify_opt_pass llmod out_file llvm_out = function
  | None       -> Log.info "Not verifying opt passes"
  | Some pass  ->
    let passes = Core.String.split pass ~on:',' in
    Log.info "Verifying opt passes `%s`" pass;
    let llmod' = Llvm_transform_utils.clone_module llmod in
    Opt.verify_some_opts llmod' passes;
    output_llvm llvm_out (out_file ^ "_optimized") llmod'

let verify_opt_passes llmod = function
  | false -> Log.info "Not verifying opt passes"
  | true  ->
    Log.info "Verifying opt passes";
    match Opt.verify_opts llmod with
      | true -> Log.error "An optimzation did not pass!"
      | false -> Log.info "All optimzations passed!"

let ctverify (Tast.Module(_,fdecs,_)) out_file llvm_mod
  (wrappers : (Tast.fun_name * Ctverif.c_code) option list) = function
  | false -> Log.info "Not verifying with ctverif!"
  | true  ->
    output_llvm true out_file llvm_mod;
    let ll_file = out_file ^ ".ll" in
    let verify f wrapper header llvm_file =
      let entrypoint = f ^ "_wrapper" in
      let cmd = [| "verif-ll.sh"; wrapper; header; ll_file; entrypoint|] in
      let ret_code = try run_command "" cmd false
        with | _ -> Log.error "Ct-verif failed to run. Please check that `docker/build-ctverif` is in you PATH"; exit 1; in
      Log.info "Ct-verif returned with status %d" ret_code;
      (* TODO: Have verif-ll.sh check if ct-verif passed or not and set the
               error code accordingly. *)
      in
    List.map (fun wrapper (*(fun_name,c_code)*) ->
      match wrapper with
        | None -> ()
        | Some({data=fun_name},c_code) ->
          let wrapper_name = fun_name ^ "_wrapper.c" in
          Core.Out_channel.write_all wrapper_name ~data:c_code;
          verify fun_name wrapper_name (out_file ^ ".h") llvm_mod
    ) wrappers |> ignore;
    ()
*)

let compile (in_files,out_file,out_dir) args =
  let out_file' = generate_out_file out_dir out_file in
  (*Log.debug "Compiling program in %s mode" (show_mode args.mode);*)
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
  let ast = Arr_speccer.transform ast in (* array lengths filled in *)
    output_ast args.ast_out out_file' ast;
  let tast = Typecheck.transform ast in (* transition to tast; exprs have types *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  (*let tast = Oobcheck.transform tast in (* array accesses etc. validated *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  let tast = Transfn.transform tast in (* transform secret fn calls *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;
  let tast = Transret.transform tast in (* transform early returns *)
    output_tast args.ast_out out_file' tast;
    generate_pseudo args.pseudo_out out_file' tast;*)
  (*generate_header (args.gen_header || args.verify_llvm) out_file' tast;
  Log.debug "Typecheck complete";
  let xftast = Transform.xf_module tast args.mode in
  Log.debug "Tast transform complete";
  let xftast = Transform_debug.xf_module args.mode xftast in
  output_xftast args.core_ir_out out_file' xftast;
  generate_pseudo args.pseudo_out out_file' xftast;
  generate_smack args out_file' xftast;
  let xftast = Oob.transform xftast in
    (* XXX *) generate_pseudo args.pseudo_out out_file' xftast;
  let xftast = Transform_args.xf_module xftast in
  let llvm_ctx = Llvm.create_context () in
  let llvm_mod = Llvm.create_module llvm_ctx "Module" in
  let llvm_builder = Llvm.builder llvm_ctx in
  let _ = codegen llvm_ctx llvm_mod llvm_builder args.verify_llvm xftast in
  
  (* TODO: The line below will generate a C wrapper to send to ct-verif.
     Uncomment it when ready. *)
  let c_wrappers = Ctverif.generate_wrappers out_file' xftast in
  List.map (fun wrapper ->
    match wrapper with
      | None -> ()
      | Some (_,c) -> ()(*Log.debug "C-wrapper:\n%s" c*))
    c_wrappers |> ignore;
  
  (* Verify the opt passes via the command line. This doesn't affect llvm_mod *)
  (* verify_opt_pass llvm_mod out_file' args.llvm_out args.verify_opts; *)

  (* Verify all of the opt passes on the IR. This doesn't affect llvm_mod *)
  (* verify_opt_passes (Llvm_transform_utils.clone_module llvm_mod) args.verify_llvm; *)

  (* Lets optimize the module *)
  let llvm_mod = Opt.run_optimizations args.opt_level args.opt_limit llvm_mod in

  ctverify xftast out_file' llvm_mod c_wrappers args.verify_llvm;

  (* Start verify final IR *)
  let errors = Hashtbl.create 100 in
  if not args.noguac then
  begin
  match Verify.verify errors "NoOpt" llvm_mod with
    | Verify.Secure -> Log.debug "Secure!"
    | Verify.InSecure ->
      let print_errors errors =
        let strings = Hashtbl.fold
        (fun (des,det) pass acc -> (pass ^ " -- " ^ des ^ " -- " ^ det)::acc)
        errors [] in
        Log.error "%s" (String.concat "\n" strings);
        begin match args.mode with
        | Debugfun.DEV ->
          output_llvm args.llvm_out out_file' llvm_mod;
          output_bitcode out_file' llvm_mod;
          output_assembly args.opt_level out_file' |> ignore;
          output_shared_object out_file' args;
          output_object out_file' |> ignore
        | Debugfun.PROD -> () end;
        exit 1 |> ignore;
        () in
      Log.error "Insecure!";
      print_errors errors;
    | Verify.Unchanged -> Log.debug "Unchanged!"
    | Verify.Unknown -> Log.debug "Unknown!" end;
  (* End verify final IR *)

  (*
  let triple = Llvm_target.Target.default_triple () in
  let lltarget = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly = Llvm_target.TargetMachine.data_layout llmachine in
  Llvm.set_target_triple (Llvm_target.TargetMachine.triple llmachine) llvm_mod;
  Llvm.set_data_layout (Llvm_target.DataLayout.as_string lldly) llvm_mod;
  *)

  (* ADD BACK: Llvm_analysis.assert_valid_module llvm_mod |> ignore;*)
  output_llvm args.llvm_out out_file' llvm_mod;
  output_bitcode out_file' llvm_mod;
  output_assembly args.opt_level out_file' |> ignore;
  output_shared_object out_file' args;
  output_object out_file' |> ignore*)
