open OUnit2
open Typecheck
open Ast

let prim = Primitive(Number 1)
let var = Variable "dne"
let bop = Binop(Plus,Primitive(Number 1), Primitive(Number 210))
let unop = Unop(B_Not,Primitive(Number 1))
let t = Primitive(Boolean true)
let f = Primitive(Boolean false)
let if' = If(f,[Return(prim)],[Return(prim)])
let if'' = If(f,[Return(prim)],[Return(t)])
let dec = VarDec("var1", Int, Primitive(Number 1))
let seqdec = [dec; Return(Variable "var1")]
let funbodyexpr = Binop(Plus, Variable("bar"), Primitive(Number 1))
let funbodystms = [VarDec("derp",Int,funbodyexpr);Return(Variable "derp")]
let fundec = FunctionDec("foo", [{name="bar"; ty=Int}], Int, funbodystms)
let funbodyexpr' = Binop(Plus, Variable("bar"), Primitive(Number 1))
let callexp = CallExp("foo", [Variable("bar")])
let funbodystms' = [VarDec("derp",Int,funbodyexpr);Return(callexp)]
let fundec' = FunctionDec("foo2", [{name="bar"; ty=Int}], Int, funbodystms')
let funcall = FDec([fundec;fundec'])

let test1 ctx = assert_equal Int (tc_expr prim)
let test2 ctx = assert_raises (VariableNotDefined "Variable not defined:\tdne") (fun () -> tc_expr var)
let test3 ctx = assert_equal Int (tc_expr bop)
let test4 ctx = assert_equal Int (tc_expr unop)
let test5 ctx = assert_equal NoneType (tc_stm if')
let test6 ctx = assert_raises (TypeError "Int does not unify with Bool") (fun () -> tc_stm if'')
let test7 ctx = assert_equal Int (tc_stms seqdec)
let test8 ctx = assert_equal NoneType (tc_fdec fundec)
let test9 ctx = assert_equal NoneType (tc_module funcall)

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
