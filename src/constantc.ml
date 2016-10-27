open Ast
open Env
open Typecheck

let body = Return (CallExp("printf", [Primitive(ByteArray("vaer"))]))

let prgm2 = FunctionDec("main", [], Int, [body])

let compile ast_module =
  let _ =  tc_module ast_module in
  let _ = Codegen.codegen ast_module in
  Llvm.print_module "out.ll" Codegen.the_module;
  ()

let run () =
  let args = Array.make 2 "" in
  let _ = Array.set args 0 "lli" in
  let _ = Array.set args 1 "out.ll" in
  Unix.execv "lli" args

let _ =
  match Sys.argv with
  | [|_;"compile"|] -> compile (FDec[prgm2]); ()
  | [|_;"run"|] -> run (); ()
  | _ -> print_string "Unknown command\n"; ()
