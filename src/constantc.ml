open Codegen2
open Ast
open Env
open Typecheck
open Transform

let body = Return (CallExp("printf", [Primitive(ByteArray("vaevaerwgr\n"))]))

let prgm2 = FunctionDec("main", [], Int, [body])

let compile ast_module =
  let _ =  tc_module ast_module in
  let core_ir = transform ast_module in
  let llvm_ctx = Llvm.create_context () in
  let llvm_mod = Llvm.create_module llvm_ctx "Module" in
  let _ = codegen llvm_ctx llvm_mod core_ir in
  Llvm.print_module "out.ll" llvm_mod;
  ()

let run () =
  let a  = Unix.fork () in
  match a with
    | 0 -> (try
          Unix.execvp "lli" [|"lli"; "out.ll"|]
       with
          _ -> Printf.printf "%s" "error while execv\n"; exit (-1))
    | -1 -> Printf.printf "%s" "error accured on fork\n"
    | _ -> ignore (Unix.wait ()); Printf.printf "%s" "Done...\n"

let _ =
  match Sys.argv with
  | [|_;"compile"|] -> compile (CModule[prgm2]); ()
  | [|_;"run"|] -> run ()
  | _ -> print_string "Unknown command\n"; ()
