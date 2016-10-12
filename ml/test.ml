open OUnit2
open Typecheck
open Ast
open Types

let prim = Primitive(Number 1)
let var = Variable "dne"
let bop = BinOp(Plus,Primitive(Number 1), Primitive(Number 210))
let unop = UnaryOp(B_Not,Primitive(Number 1))
let t = Primitive(Bool true)
let f = Primitive(Bool false)
let if' = If(f,prim,prim)
let if'' = If(f,prim,t)
let dec = Dec(VarDec("var1", Primitive(Number 1)))
let seqdec = Seq(dec, Variable "var1")
let funbody = BinOp(Plus, Variable("bar"), Primitive(Number 1))
let fundec = Dec(FunctionDec("foo", [{name="bar"; ty=Int}], funbody))
let funcall = Seq(fundec, CallExp("foo", [Primitive(Number 1)]))
let funcall' = Seq(fundec, CallExp("foo", [Primitive(Bool true)]))

let test1 ctx = assert_equal Int (tc_expr prim).ty
let test2 ctx = assert_raises (VariableNotDefined "Variable not defined:\tdne") (fun () -> tc_expr var)
let test3 ctx = assert_equal Int (tc_expr bop).ty
let test4 ctx = assert_raises (UnknownType "Type is unknown for boolean operator:\t ~") (fun () -> tc_expr unop)
let test5 ctx = assert_equal Int (tc_expr if').ty
let test6 ctx = assert_raises (TypeError "Int does not unify with Bool") (fun () -> tc_expr if'')
let test7 ctx = assert_equal Int (tc_expr seqdec).ty
let test8 ctx = assert_equal Null (tc_expr fundec).ty
let test9 ctx = assert_equal Int (tc_expr funcall).ty
let test10 ctx = assert_raises (TypeError "Bool does not unify with Int") (fun () -> (tc_expr funcall').ty)


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
     "test9">:: test9;
     "test10">:: test10;];;

let () = run_test_tt_main suite
