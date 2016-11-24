open Ast
open Command_util

(* Program 1 *)
let dec = VarDec("x", Int, Primitive(Number 666))
let assign = Assign("x", Primitive(Number 10))
let dec' = VarDec("x2", Int, Primitive(Number 100))
let ret = Return(VarExp "x2")
let prgm1 = FunctionDec("get100", [], Int, [dec;dec';assign;ret])

(* Array Access *)
let l = List.map (fun c -> Char.code c) ['h';'e';'l';'l';'o';' ';'w';'o';'r';'l';'d']
let bytearrdec = VarDec("arr", ByteArr(11), Primitive(ByteArray l))
let ret' = Return(ArrExp("arr", 4))
let prgm2 = FunctionDec("getByteArrIndex", [], Int, [bytearrdec;ret'])

(* Array Set *)
let assign = ArrAssign("arr",4,Primitive(Number 44))
let prgm3 = FunctionDec("setByteArrIndex", [], Int, [bytearrdec;assign;ret'])

(* Int arg *)
let int_param = { name="my_int"; ty=Int }
let ret''' = Return(VarExp "my_int")
let prgm4 = FunctionDec("identity", [int_param], Int, [ret'''])

(* Array arg and set *)
let arr_param = { name="arr"; ty=ByteArr(5) }
let prgm5 = FunctionDec("mutateArray", [arr_param], Int, [assign;ret'])

(* Complex array arg set *)
let arr_param' = { name="arr2"; ty=ByteArr(5) }
let val_param = { name="val"; ty=Int }
let assign' = ArrAssign("arr2",4,VarExp("val"))
let ret'' = Return(ArrExp("arr2", 4))
let prgm6 = FunctionDec("mutateArray2", [arr_param';val_param], Int, [assign';ret''])

(* Simple If *)
let if_arg = { name="cond"; ty=Int }
let cond = BinOp(GT, Primitive(Number 10), VarExp("cond"))
let then' = Return(Primitive(Number 1))
let else' = Return(Primitive(Number 2))
let if' = If(cond,[then'],[else'])
let prgm7 = FunctionDec("simpleIf",[if_arg],Int,[if'])


(* Module 1 *)
let m1 = CModule [prgm1;prgm2;prgm3;prgm4;prgm5;prgm6;prgm7]

(* List of modules to test *)
let programs = [m1]

let test p =
  let _ = compile p in
  let _ = List.map (fun c -> c ()) commands in ()

let _ = ignore(List.map test programs)
