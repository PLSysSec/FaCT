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
let bytearr = Primitive(ByteArray [1;2;3;4;5;6;7;8])
let for' = For("i",Number 0,Number 1,[])
let for'' = For("i",Number 1,Number 1,[])
let for''' = For("i",Number 11,Number 1,[])
let for'''' = For("i",ByteArray[1],Number 1,[])

let venv = Hashtbl.create 10
let fn_ty = Int
let test01 ctx = assert_equal Int (tc_expr venv prim)
let test02 ctx = assert_raises
    (VariableNotDefined "Variable not defined:\tdne")
    (fun () -> tc_expr venv var)
let test03 ctx = assert_equal Int (tc_expr venv bop)
let test04 ctx = assert_equal Int (tc_expr venv unop)
let test05 ctx = assert_equal () (tc_stm fn_ty venv if')
let test06 ctx = assert_raises
    (TypeError "Int does not unify with Bool")
    (fun () -> tc_stm fn_ty venv if'')
let test07 ctx = assert_equal () (tc_stms fn_ty venv seqdec)
let test08 ctx = assert_equal () (tc_fdec venv fundec)
let test09 ctx = assert_equal (ByteArr 8) (tc_expr venv bytearr)
let test10 ctx = assert_equal () (tc_stm fn_ty venv for')
let test11 ctx = assert_raises
    (ForError "Low value must be smaller than high value in for loop")
    (fun () -> (tc_stm fn_ty venv for''))
let test12 ctx = assert_raises
    (ForError "Low value must be smaller than high value in for loop")
    (fun () -> (tc_stm fn_ty venv for'''))
let test13 ctx = assert_raises
    (TypeError "ByteArr does not unify with Int")
    (fun () -> tc_stm fn_ty venv for'''')

let suite =
  "basic typecheck tests">:::
  ["test01">:: test01;
   "test02">:: test02;
   "test03">:: test03;
   "test04">:: test04;
   "test05">:: test05;
   "test06">:: test06;
   "test07">:: test07;
   "test08">:: test08;
   "test09">:: test09;
   "test10">:: test10;
   "test11">:: test11;
   "test12">:: test12;
   "test13">:: test13;
   ];;

let () = run_test_tt_main suite
