open Llvm
open Ast
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

(*
  int ssl3_cbc_remove_padding(const SSL *s,
                              SSL3_RECORD *rec,
                              unsigned block_size, unsigned mac_size)
  {
      unsigned padding_length, good;
      const unsigned overhead = 1 /* padding length byte */  + mac_size;

      /*
       * These lengths are all public so we can test them in non-constant time.
       */
      if (overhead > rec->length)
          return 0;

      //padding_length = rec->data[rec->length - 1];
      padding_length = 4;

      good = constant_time_ge(rec->length, padding_length + overhead);
      /* SSLv3 requires that the padding is minimal. */
      good &= constant_time_ge(block_size, padding_length + 1);
      padding_length = good & (padding_length + 1);
      rec->length -= padding_length;
      rec->type |= padding_length << 8; /* kludge: pass padding length */
      return constant_time_select_int(good, 1, -1);
  }


  int ssl3_cbc_remove_padding2(unsigned int length, unsigned char *data, int type, unsigned char *input,
                              unsigned block_size, unsigned mac_size)
  {
      unsigned padding_length = 4, good; // formerly: padding_length = rec->data[rec->length - 1];
      const unsigned overhead = 1 /* padding length byte */  + mac_size;

      /*
       * These lengths are all public so we can test them in non-constant time.
       */
      if (overhead > length)
          return 0;

      good = (length >= padding_length+overhead) && (block_size >= padding_length + 1);

      if(good) padding_length = padding_length + 1;
      else padding_length = 0;

      length = length - padding_length;
      type = type | (padding_length << 8); // doing (padding_length * 256) instad of (padding_length << 8); jk doing (padding_length + 420)

      if(good) return 1;
      else return -1;
  }
*)

let ssl3_cbc_remove_padding = FunctionDec("ssl3_cbc_remove_padding",
  [{name="length"; ty=Int}; {name="data"; ty=(ByteArr 1024)}; {name="type"; ty=Int};
  {name="input"; ty=(ByteArr 1024)}; {name="block_size"; ty=Int};
  {name="mac_size"; ty=Int}], Int,
  [
  VarDec("padding_length", Int, Primitive(Number 4));
  VarDec("good", Bool, Primitive(Number 0));
  VarDec("overhead", Int, BinOp(Plus, Primitive(Number 1), VarExp("mac_size")));

  (* should skip in else branch *)
  If(BinOp(GT, VarExp("overhead"), VarExp("length")), [Return(Primitive(Number 0))], [Return(Primitive(Number 0))]);

  (* should be GTE instead of GT *)
  Assign("good", BinOp(B_And,
    BinOp(GT, VarExp("length"), BinOp(Plus, VarExp("padding_length"), VarExp("overhead"))),
    BinOp(GT, VarExp("block_size"), BinOp(Plus, VarExp("padding_length"), Primitive(Number 1)))));

  If(VarExp("good"),
    [Assign("padding_length", BinOp(Plus, VarExp("padding_length"), Primitive(Number 1)))],
    [Assign("padding_length", Primitive(Number 0))]);

  Assign("length", BinOp(Minus, VarExp("length"), VarExp("padding_length")));
  (* should shift left intead of multiply *)
  Assign("type", BinOp(B_Or, VarExp("type"), BinOp(Plus, VarExp("padding_length"), Primitive(Number 420))));

  If(VarExp("good"),
    [Return(Primitive(Number 1))],
    [Return(Primitive(Number (-1)))]);

  ]);;

module SS = Set.Make(String);;
let main ast =
  print_string "Compiling constantc...\n";
  let _ = codegen ast in
  print_module "out.ll" the_module;
  ()
;;

main (FDec [ssl3_cbc_remove_padding]);;
