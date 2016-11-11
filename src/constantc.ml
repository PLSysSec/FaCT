open Codegen2
open Ast
open Env
open Typecheck
open Transform

let dec = VarDec("x", Int, Primitive(Number 666))
let assign = Assign("x", Primitive(Number 10))
let ret = Return(VarExp "x")
let prgm2 = FunctionDec("get10", [], Int, [dec;assign;ret])

let run_command c args =
  let a  = Unix.fork () in
  match a with
  | 0 -> (try
            Unix.execvp c args
          with
            _ -> Printf.printf "%s" "error while execv\n"; exit (-1))
  | -1 -> Printf.printf "%s" "error accured on fork\n"
  | _ -> ignore (Unix.wait ()); Printf.printf "%s" "Done...\n"

let compile ast_module =
  let _ =  tc_module ast_module in
  let core_ir = transform ast_module in
  let _ = Format.printf "core ir: %s\n" (Cast.show_cmodule core_ir) in
  let llvm_ctx = Llvm.create_context () in
  let llvm_mod = Llvm.create_module llvm_ctx "Module" in
  let _ = codegen llvm_ctx llvm_mod core_ir in
  Llvm.print_module "out.ll" llvm_mod;
  ()

let compile' = (fun () -> ignore(compile (CModule[prgm2])))
let run = (fun () -> run_command "lli" [|"lli"; "out.ll"|])
let link = (fun () -> run_command "llvm-as" [|"llvm-as"; "out.ll"|])
let assemble = (fun () -> run_command "llc" [|"llc"; "out.bc"|])
let share = (fun () -> run_command "clang" [|"clang"; "-c"; "out.s"|])
let harness =
  (fun () -> run_command "gcc" [|"gcc"; "-o"; "final"; "harness.o"; "out.o"|])
let clean = (fun () ->
    run_command "rm" [|"rm"; "out.ll"; "out.bc"; "out.o"; "out.s"; "final"|])

let commands = [compile'; run; link; assemble; share; harness;]

let _ =
  match Sys.argv with
  | [|_;"compile"|] ->  compile' ()
  | [|_;"run"|] -> run ()
  | [|_;"link"|] -> link ()
  | [|_;"assemble"|] -> assemble ()
  | [|_;"share"|] -> share ()
  | [|_;"harness"|] -> harness ()
  | [|_;"clean"|] -> clean ()
  | [|_;"all"|] -> ignore(List.map (fun c -> c ()) commands)
  | _ -> print_string "Unknown command\n"; ()
