open Ast
open Command_util


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
  VarDec("good1", Bool, Primitive(Boolean false));
  VarDec("good2", Bool, Primitive(Boolean false));
  VarDec("overhead", Int, BinOp(Plus, Primitive(Number 1), VarExp("mac_size")));

  If(BinOp(GT, VarExp("overhead"), VarExp("length")), [Return(Primitive(Number 0))], [Assign("good1", Primitive(Boolean false))]);

  Assign("good1", BinOp(GTE, VarExp("length"), BinOp(Plus, VarExp("padding_length"), VarExp("overhead"))));
  Assign("good2", BinOp(GTE, VarExp("block_size"), BinOp(Plus, VarExp("padding_length"), Primitive(Number 1))));

  If(VarExp("good1"),
    [If(VarExp("good2"),
      [Assign("padding_length", BinOp(Plus, VarExp("padding_length"), Primitive(Number 1)))],
      [Assign("padding_length", Primitive(Number 0))])],
    [Assign("padding_length", Primitive(Number 0))]);

  (*Assign("length", BinOp(Minus, VarExp("length"), VarExp("padding_length")));

  Assign("type", BinOp(B_Or, VarExp("type"), BinOp(LeftShift, VarExp("padding_length"), Primitive(Number 8))));
  *)
  If(VarExp("good1"),
    [If(VarExp("good2"),
      [Return(Primitive(Number 1))],
      [Return(Primitive(Number (-1)))])],
    [Return(Primitive(Number (-1)))]);
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
