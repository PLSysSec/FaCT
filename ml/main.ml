open Llvm
open Ast
open Constpass
open OUnit2
open Codegen
open Typecheck
open Types

let basicAst = Primitive(Number 1);;

let inner = BinOp(Plus,Primitive(Number 92), Primitive(Number 90));;

let ast1 = (BinOp (Plus,inner,Primitive(Number 1)));;

let cond = (BinOp (GT,(Variable "x"),(Primitive(Number 5))));;

let y = (Variable "y");;

let then_branch = Mutate(y,(BinOp(Plus,y,Primitive(Number 2))));;

let else_branch = Mutate(y,(BinOp(Minus,y,Primitive(Number 7))));;

let if_statement = If(cond,then_branch,else_branch);;

let dec = Dec(VarDec("y",(Primitive(Number 3))));;

let dec_and_if = Seq(dec,if_statement);;

let param1 = { name="param1"; ty=Int };;
let param2 = { name="param2"; ty=Int };;
let body = ast1;;
let ast3 = FunctionDec("myfunc", [param1;param2], dec_and_if);;


let boolify_param = { name="boolify_param"; ty=Bool };;
let boolify_body = Variable "boolify_param";;
let boolify = FunctionDec("BOOLIFY", [boolify_param], boolify_body);;

let identity_param = { name="identity_param"; ty=Int };;
let identity_body = Variable "identity_param";;
let identity_body' = CallExp("BOOLIFY", [Variable "identity_param"])
let identity = FunctionDec("Identity", [identity_param], identity_body');;

let original = Seq (
  Dec(VarDec("y", Primitive(Number(3)))),
  Seq(If(BinOp(Plus,Variable("x"),Primitive(Number 5)), (* TODO: change Plus to GT. This gives a type error because Boolify expects a boolean *)
        Mutate(Variable("y"), BinOp(Plus,Variable("y"),Primitive(Number(2)))),
        Mutate(Variable("y"), BinOp(Minus,Variable("y"),Primitive(Number(-7))))),
      Return(Variable "y")));;

let mutated = Seq (
  Dec(VarDec("x_g_5", CallExp("BOOLIFY", [BinOp(Plus,Variable("x"),Primitive(Number 5))]))),
  Seq(Dec(VarDec("y", (BinOp (Plus,
                      Primitive(Number 3),
                      BinOp(Plus,
                            BinOp(B_And,Primitive(Number 2),Variable("x_g_5")),
                            BinOp(B_And,Primitive(Number (-7)), UnaryOp(B_Not,Variable("x_g_5")))))))),
      Seq(CallExp("printf", [Primitive(Number 19)]),
          Return(Variable "y")))
);;

let mut_fun = FunctionDec("main",[{ name="x"; ty=Int }], mutated);;

module SS = Set.Make(String);;
let main ast =
  print_string "Compiling constantc...\n";
  let _ = codegen ast in
  dump_module the_module;
  ()
;;

(*main (Seq (ast1::ast2::[]))
main (Seq (Dec boolify, Dec identity));;*)
main (Seq (Dec boolify, Dec mut_fun));;

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
