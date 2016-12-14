open Ast

let eval = function
  | BinOp(b,Primitive(Number l),Primitive(Number r)) ->
    begin
      match b with
      | Plus -> l + r
      | Minus -> l - r
      | Multiply -> l * r
      | _ -> -1
    end;
  | _ -> -1

let _ =
  Lexer.file := Some "derp.txt";
  let lexbuf = Lexing.from_channel (open_in "derp.txt") in
  let result = Parser.main Lexer.token lexbuf in
  print_string "SIZE:\t";
  print_int (List.length result);
  print_string "DONE:\n";
  ignore(List.map (fun f -> Format.printf "%s\n" (show_fdec f)) result);
  print_newline();
