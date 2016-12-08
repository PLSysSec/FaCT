open Cast
open Codegen
open Ast
open Env
open Typecheck
open Transform

let run_command c args =
  let a  = Unix.fork () in
  match a with
  | 0 -> (try
            Unix.execvp c args
          with
            _ -> Printf.printf "%s" "error while execv\n"; exit (-1))
  | -1 -> Printf.printf "%s" "error accured on fork\n"
  | _ -> ignore (Unix.wait ())

let compile ast_module =
  let _ =  tc_module ast_module in
  let core_ir = transform ast_module in
  let llvm_ctx = Llvm.create_context () in
  let llvm_mod = Llvm.create_module llvm_ctx "Module" in
  let _ = codegen llvm_ctx llvm_mod core_ir in
  Llvm.print_module "out.ll" llvm_mod;
  ()

let compile' ast = ignore(compile ast)
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
