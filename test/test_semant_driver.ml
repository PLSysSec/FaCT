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
let ret' = Return(ArrExp("arr", Primitive(Number 4)))
let prgm2 = FunctionDec("getByteArrIndex", [], Int, [bytearrdec;ret'])

(* Array Set *)
let assign = ArrAssign("arr",Primitive(Number 4),Primitive(Number 44))
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
let assign' = ArrAssign("arr2",Primitive(Number 4),VarExp("val"))
let ret'' = Return(ArrExp("arr2", Primitive(Number 4)))
let prgm6 = FunctionDec("mutateArray2", [arr_param';val_param], Int, [assign';ret''])

(* Simple If *)
let if_arg = { name="cond"; ty=Int }
let cond = BinOp(GT, Primitive(Number 10), VarExp("cond"))
let then' = Return(Primitive(Number 1))
let else' = Return(Primitive(Number 2))
let if' = If(cond,[then'],[else'])
let prgm7 = FunctionDec("simpleIf",[if_arg],Int,[if'])

(* Medium Complex If *)
let if_dec = VarDec("complex_ret", Int, Primitive(Number 10))
let then'' = Assign("complex_ret", Primitive(Number 1))
let else'' = Assign("complex_ret", Primitive(Number 2))
let if'' = If(cond,[then''],[else''])
let ret''' = Return(VarExp "complex_ret")
let prgm8 = FunctionDec("mediumComplexIf", [if_arg], Int, [if_dec;if'';ret'''])

(* Mixed If *)
let cond' = BinOp(Equal, Primitive(Number 10), VarExp("cond"))
let if''' = If(cond',[then'],[else''])
let prgm9 = FunctionDec("mixedIf", [if_arg], Int, [if_dec;if''';ret'''])

(* Mixed If 2 *)
let if4 = If(cond,[then''],[else'])
let prgm10 = FunctionDec("mixedIf2", [if_arg], Int, [if_dec;if4;ret'''])

(* Nested If *)
let cond_a = BinOp(GT, Primitive(Number 5), VarExp("cond"))
let cond_b = BinOp(GT, Primitive(Number 15), VarExp("cond"))
let ret1 = Return(Primitive(Number 1))
let ret2 = Return(Primitive(Number 2))
let ret3 = Return(Primitive(Number 3))
let ret4 = Return(Primitive(Number 4))
let if_a = If(cond_a,[ret1],[ret2])
let if_b = If(cond_b,[ret3],[ret4])
let nif = If(cond,[if_a],[if_b])
let prgm11 = FunctionDec("nestedIf", [if_arg], Int, [nif])

(* Simple for loop *)
let body = VarDec("a",Int,Primitive(Number 666))
let loop = For("i",Number 0,Number 10,[body])
let ret'''' = Return(Primitive(Number 10000))
let prgm12 = FunctionDec("simpleLoop",[],Int,[loop;ret''''])

(* Loop acc *)
let acc_dec = VarDec("acc", Int, Primitive(Number 0))
let loop_acc_body = Assign("acc", BinOp(Plus, VarExp("acc"), Primitive(Number 1)))
let loop_acc = For("i",Number 0,Number 5,[loop_acc_body])
let acc_ret = Return(VarExp "acc")
let prgm13 = FunctionDec("loopAcc",[],Int,[acc_dec;loop_acc;acc_ret])

(* Set array in loop *)
(* TODO: Byte arrays should have access to this variable *)
let loop_body = ArrAssign("arr", VarExp("i"), VarExp("i"))
let loop' = For("i",Number 0,Number 5,[loop_body])
let prgm14 = FunctionDec("loopAssignArray",[arr_param],Int,[loop';ret''''])

(* Add function *)
let add_body = Return(BinOp(Plus,VarExp("a"),VarExp("b")))
let add_arg_a = { name="a"; ty=Int }
let add_arg_b = { name="b"; ty=Int }
let prgm15 = FunctionDec("add",[add_arg_a;add_arg_b],Int,[add_body])

(* Add 10 and 20 using the add function *)
let add_body' =
  Return(CallExp("add",[Primitive(Number 10);Primitive(Number 20)]))
let prgm16 = FunctionDec("add10And20",[],Int,[add_body'])

(* Add all numbers in a given byte array *)
let arr_arg = { name="bytearr"; ty=ByteArr 5}
let acc_dec = VarDec("acc", Int, Primitive(Number 0))
let acc_body = Assign("acc",BinOp(Plus,VarExp("acc"),ArrExp("bytearr",VarExp("i"))))
let acc_loop = For("i",Number 0,Number 5,[acc_body])
let ret_acc = Return(VarExp "acc")
let prgm17 = FunctionDec("addAll",[arr_arg],Int,[acc_dec;acc_loop;ret_acc])

(* Call addAll *)
let byte_arr = VarDec("arr",ByteArr 5,Primitive(ByteArray[1;1;1;1;1]))
let add_all = Return(CallExp("addAll",[VarExp("arr")]))
let prgm18 = FunctionDec("callAddAll",[],Int,[byte_arr;add_all])

(* Multiply *)
let mul_arg1 = { name="a"; ty=Int }
let mul_arg2 = { name="b"; ty=Int }
let mul = Return(BinOp(Multiply,VarExp("a"),VarExp("b")))
let prgm19 = FunctionDec("multiply",[mul_arg1;mul_arg2],Int,[mul])

(* Equal *)
let eq_arg1 = { name="a"; ty=Int }
let eq_arg2 = { name="b"; ty=Int }
let eq = Return(BinOp(Equal,VarExp("a"),VarExp("b")))
let prgm20 = FunctionDec("equal",[eq_arg1;eq_arg2],Bool,[eq])

(* Not Equal *)
let neq_arg1 = { name="a"; ty=Int }
let neq_arg2 = { name="b"; ty=Int }
let neq = Return(BinOp(NEqual,VarExp("a"),VarExp("b")))
let prgm21 = FunctionDec("nequal",[neq_arg1;neq_arg2],Bool,[neq])

(* Left Shift *)
let ls_arg1 = { name="num"; ty=Int }
let ls_arg2 = { name="shift"; ty=Int }
let lshift = Return(BinOp(LeftShift,VarExp("num"),VarExp("shift")))
let prgm22 = FunctionDec("lshift",[ls_arg1;ls_arg2],Int,[lshift])

(* Left Shift *)
let rs_arg1 = { name="num"; ty=Int }
let rs_arg2 = { name="shift"; ty=Int }
let rshift = Return(BinOp(RightShift,VarExp("num"),VarExp("shift")))
let prgm23 = FunctionDec("rshift",[rs_arg1;rs_arg2],Int,[rshift])

(* Module 1 *)
let m1 = CModule
    [prgm1;
     prgm2;
     prgm3;
     prgm4;
     prgm5;
     prgm6;
     prgm7;
     prgm8;
     prgm9;
     prgm10;
     prgm11;
     prgm12;
     prgm13;
     prgm14;
     prgm15;
     prgm16;
     prgm17;
     prgm18;
     prgm19;
     prgm20;
     prgm21;
     prgm22;
     prgm23]

(* List of modules to test *)
let programs = [m1]

let test p =
  let _ = compile p in
  let _ = List.map (fun c -> c ()) commands in ()

let _ = ignore(List.map test programs)
