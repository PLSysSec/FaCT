open Ast
open Command_util


let ssl3_cbc_remove_padding = FunctionDec("ssl3_cbc_remove_padding",
  [{name="data"; ty=(ByteArr 1024)};
  {name="input"; ty=(ByteArr 1024)}; {name="block_size"; ty=Int};
  {name="mac_size"; ty=Int}; {name="lengthtype_array"; ty=(ByteArr 2)}], Int,
  [
  VarDec("padding_length", Int, Primitive(Number 4));
  VarDec("overhead", Int, BinOp(Plus, Primitive(Number 1), VarExp("mac_size")));
  VerDec("dummy", Int, Primitive(Number 0));

  If(BinOp(GT, VarExp("overhead"), ArrExp("lengthtype_array", Primitive(Number 0))),
    [Return(Primitive(Number 0))],
    [Assign("dummy", Primitive(Number 0))]);

  If(BinOp(LT, ArrExp("lengthtype_array", Primitive(Number 0)), BinOp(Plus, VarExp("padding_length"), VarExp("overhead"))),
    [Return(Primitive(Number -1))],
    [Assign("dummy", Primitive(Number 0))]);
  If(BinOp(LT, VarExp("block_size"), BinOp(Plus, VarExp("padding_length"), Primitive(Number 1))),
    [Return(Primitive(Number -1))],
    [Assign("dummy", Primitive(Number 0))]);

  [Assign("padding_length", BinOp(Plus, VarExp("padding_length"), Primitive(Number 1)))];

  ArrAssign("lengthtype_array", Primitive(Number 0), BinOp(Minus, ArrExp("lengthtype_array", Primitive(Number 0)), VarExp("padding_length")));
  ArrAssign("lengthtype_array", Primitive(Number 1),  BinOp(B_Or, ArrExp("lengthtype_array", Primitive(Number 1)), BinOp(LeftShift, VarExp("padding_length"), Primitive(Number 8))));

  [Return(Primitive(Number 1))];
  ]);;



(* Module 1 *)
let m1 = CModule
    [ssl3_cbc_remove_padding]

(* List of modules to test *)
let programs = [m1]
let commands = [run; link; assemble; share; compile_ssl; compile_c "ssl.o";]

let test p =
  let _ = compile p in
  let _ = List.map (fun c -> c ()) commands in ()

let _ = ignore(List.map test programs)
