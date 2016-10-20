open Llvm
open Ast
(*open Constpass*)
open OUnit2
(*open Codegen*)
(*open Typecheck*)
open Types

let basicAst = Primitive(Number 1);;

let inner = BinOp(Plus,Primitive(Number 92), Primitive(Number 90));;

let ast1 = (BinOp (Plus,inner,Primitive(Number 1)));;

let cond = (BinOp (GT,(VarExp "x"),(Primitive(Number 5))));;

let y = (VarExp "y");;

let then_branch = Assign("y",(BinOp(Plus,y,Primitive(Number 2))));;

let else_branch = Assign("y",(BinOp(Minus,y,Primitive(Number 7))));;

let if_statement = If(cond,[then_branch],[else_branch]);;

(*
int func (Bool param) {
    Bool foo = param;
}
*)
let my_param = { name="param"; ty=Bool };;
let my_body = VarDec("foo", Bool, VarExp "param")
let my_func = FunctionDec("func", [my_param], Int, [my_body]);;

(*
let identity_param = { name="identity_param"; ty=Int };;
let identity_body = VarExp "identity_param";;
let identity_body' = CallExp("BOOLIFY", [VarExp "identity_param"])
let identity = FunctionDec("Identity", [identity_param], identity_body');;
*)

(*
let original = Seq (
  Dec(VarDec("y", Primitive(Number(3)))),
  Seq(If(BinOp(Plus,VarExp("x"),Primitive(Number 5)), (* TODO: change Plus to GT. This gives a type error because Boolify expects a boolean *)
        Assign(VarExp("y"), BinOp(Plus,VarExp("y"),Primitive(Number(2)))),
        Assign(VarExp("y"), BinOp(Minus,VarExp("y"),Primitive(Number(-7))))),
      Return(VarExp "y")));;

let mutated = Seq (
  Dec(VarDec("x_g_5", CallExp("BOOLIFY", [BinOp(Plus,VarExp("x"),Primitive(Number 5))]))),
  Seq(Dec(VarDec("y", (BinOp (Plus,
                      Primitive(Number 3),
                      BinOp(Plus,
                            BinOp(B_And,Primitive(Number 2),VarExp("x_g_5")),
                            BinOp(B_And,Primitive(Number (-7)), UnaryOp(B_Not,VarExp("x_g_5")))))))),
      Seq(CallExp("printf", [Primitive(Number 19)]),
          Return(VarExp "y")))
);;
*)

(*
let mut_fun = FunctionDec("main",[{ name="x"; ty=Int }], mutated);;

module SS = Set.Make(String);;
let main ast =
  print_string "Compiling constantc...\n";
  let _ = codegen ast in
  print_module "out.ll" the_module;
  ()
;;

(*main (Seq (ast1::ast2::[]))
main (Seq (Dec boolify, Dec identity));;*)
main (Seq (Dec boolify, Dec mut_fun));;
*)
(*
int simple(int x) {
  int y = 3;
  if(x > 5) {
    y +=2;
  } else {
    y -= 7;
  }
  return y;
}

}

int simple(int x) {
  int y = 3;
  int x_g_5 = BOOLIFY(x > 5);
  y = y + (2 & x_g_5) + (-7 & ~x_g_5);
  return y;
}

*)
