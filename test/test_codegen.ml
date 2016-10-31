open OUnit2
open Ast
open Codegen
open Llvm

let prgm1 = FDec([FunctionDec("prgm1", [{name="x"; ty=Int}], Int, [
  VarDec("y", Int, Primitive(Number 2));
  Return (VarExp "y")])]);;

let prgm2 = FDec([FunctionDec("prgm2", [{name="x"; ty=Int}], Int, [
  VarDec("y", Int, Primitive(Number 2));
  Return (VarExp "y")])]);;

let ctx = global_context ()
let m = create_module ctx "Test codegen"
let a = (codegen ctx m prgm1);;
let ctx' = global_context ()
let m' = create_module ctx' "Test codegen"
let b = (codegen ctx' m' prgm1);;

let test1 ctx = assert_equal (string_of_llmodule m) (string_of_llmodule m')

let suite =
  "codegen tests">:::
  ["test1">:: test1;];;

let () = run_test_tt_main suite
