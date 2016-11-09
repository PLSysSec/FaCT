open Llvm
open Ast
open Constpass
open Codegen
open Typecheck

let basicAst = Primitive(Number 1);;

let inner = BinOp(Plus,Primitive(Number 92), Primitive(Number 90));;

let ast1 = (BinOp (Plus,inner,Primitive(Number 1)));;

let cond = (BinOp (GT,(VarExp "x"),(Primitive(Number 5))));;

let y = (VarExp "y");;

let then_branch = Assign("y",(BinOp(Plus,y,Primitive(Number 2))));;

let else_branch = Assign("y",(BinOp(Minus,y,Primitive(Number 7))));;

let if_statement = If(cond,[then_branch],[else_branch]);;

let hello = Primitive(ByteArray "hello");;

let letter_e = hello;;

(*let exclamation = chr (33);;*)
(*let numbers = Primitive(ByteArray chr(5));;*)

(*{1, 2, 3, 4}*)
(*
int func (Bool param) {
    Bool foo = param;
}
*)
let boolify_param = { name="boolify_param"; ty=Bool };;
let boolify_body = VarDec("new_var", Bool, VarExp "boolify_param");;
let boolify = FunctionDec("BOOLIFY", [boolify_param], Int, [boolify_body]);;


(*
Int prgm1(Int x) {
  Int y = 2;
  if(x > 5) {
    y += 3;
  else {
    y -= 7;
  }
  return y;
}
*)
let prgm1 = FunctionDec("prgm1", [{name="x"; ty=Int}], Int, [
  VarDec("y", Int, Primitive(Number 2));
  If(BinOp(GT, VarExp "x", Primitive(Number 5)),
    [Assign("y", BinOp(Plus, VarExp "y", Primitive(Number 3)))],
    [Assign("y", BinOp(Minus, VarExp "y", Primitive(Number 7)))]);
  Return (VarExp "y")]);;


(*
int prgm1_const(int x) {
  Int y = 2;
  Int x_g_5 = BOOLIFY(x > 5);
  y = y + ((3 & x_g_5) + (-7 & ~x_g_5));
  return y;
}
*)
let prgm1_const = FunctionDec("prgm1_const", [{name="x"; ty=Int}], Int, [
  VarDec("y", Int, Primitive(Number 2));
  VarDec("x_g_5", Int, CallExp("BOOLIFY", [BinOp(GT,VarExp("x"),Primitive(Number 5))]));
  Assign("y", BinOp(Plus,
    VarExp "y",
    BinOp(Plus,
      BinOp(B_And,
        Primitive(Number 3),
        VarExp "x_g_5"),
      BinOp(B_And,
        Primitive(Number (-7)),
        Unop(B_Not, VarExp "x_g_5")
      )
    )
  ));
  Return (VarExp "y")]);;

(*
Int prgm2(Int x) {
  Int y = 2;
  Int z = 4;
  if(x > 5) {
    y += 3;
  else {
    z -= 7;
  }
  return y + z;
}
*)
let prgm2 = FunctionDec("prgm2", [{name="x"; ty=Int}], Int, [
  VarDec("y", Int, Primitive(Number 2));
  VarDec("z", Int, Primitive(Number 4));
  If(BinOp(GT, VarExp "x", Primitive(Number 5)),
    [Assign("y", BinOp(Plus, VarExp "y", Primitive(Number 3)))],
    [Assign("z", BinOp(Minus, VarExp "z", Primitive(Number 7)))]);
  Return (BinOp(Plus, VarExp "y", VarExp  "z"))]);;

module SS = Set.Make(String);;
let main ast =
  print_string "Compiling constantc...\n";
  let _ = codegen ast in
  print_module "out.ll" the_module;
  ()
;;

main (FDec [prgm1]);;
