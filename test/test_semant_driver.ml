open Ast
open Command_util

(* Program 1 *)
let dec = VarDec("x", Int, Primitive(Number 666))
let assign = Assign("x", Primitive(Number 10))
let dec' = VarDec("x2", Int, Primitive(Number 100))
let ret = Return(VarExp "x2")
let prgm1 = FunctionDec("get100", [], Int, [dec;dec';assign;ret])

(* Program 2 *)
let bytearrdec = VarDec("arr", ByteArr(1), Primitive(ByteArray "a"))
let ret' = Return(Primitive(Number 0))
let prgm2 = FunctionDec("getByteArr", [], Int, [bytearrdec;ret'])

(* Module 1 *)
let m1 = CModule [prgm1;prgm2]

(* List of modules to test *)
let programs = [m1]

let test p =
  let _ = compile p in
  let _ = List.map (fun c -> c ()) commands in ()

let _ = ignore(List.map test programs)
