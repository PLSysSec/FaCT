open OUnit2
open Typecheck
open Ast
open Codegen

let prim = Primitive(Number 1)
let var = VarExp "dne"
let bop = BinOp(Plus,Primitive(Number 1), Primitive(Number 210))
let unop = Unop(B_Not,Primitive(Number 1))
let t = Primitive(Boolean true)
let f = Primitive(Boolean false)
let if' = If(f,[Return(prim)],[Return(prim)])
let if'' = If(f,[Return(prim)],[Return(t)])
let dec = VarDec("var1", Int, Primitive(Number 1))
let seqdec = [dec; Return(VarExp "var1")]
let funbodyexpr = BinOp(Plus, VarExp("bar"), Primitive(Number 1))
let funbodystms = [VarDec("derp",Int,funbodyexpr);Return(VarExp "derp")]
let fundec = FunctionDec("foo", [{name="bar"; ty=Int}], Int, funbodystms)
let funbodyexpr' = BinOp(Plus, VarExp("bar"), Primitive(Number 1))
let callexp = CallExp("foo", [VarExp("bar")])
let funbodystms' = [VarDec("derp",Int,funbodyexpr);Return(callexp)]
let fundec' = FunctionDec("foo2", [{name="bar"; ty=Int}], Int, funbodystms')
let funcall = FDec([fundec;fundec'])

let venv = Hashtbl.create 10
let fn_ty = Int
let test1 ctx = assert_equal Int (tc_expr venv prim)
let test2 ctx = assert_raises (VariableNotDefined "Variable not defined:\tdne") (fun () -> tc_expr venv var)
let test3 ctx = assert_equal Int (tc_expr venv bop)
let test4 ctx = assert_equal Int (tc_expr venv unop)
let test5 ctx = assert_equal () (tc_stm fn_ty venv if')
let test6 ctx = assert_raises (TypeError "Int does not unify with Bool") (fun () -> tc_stm fn_ty venv if'')
let test7 ctx = assert_equal () (tc_stms fn_ty venv seqdec)
let test8 ctx = assert_equal () (tc_fdec venv fundec)
let test9 ctx = assert_equal () (tc_module funcall)

let suite =
  "basic typecheck tests">:::
    ["test1">:: test1;
     "test2">:: test2;
     "test3">:: test3;
     "test4">:: test4;
     "test5">:: test5;
     "test6">:: test6;
     "test7">:: test7;
     "test8">:: test8;
     "test9">:: test9;];;

let () = run_test_tt_main suite
