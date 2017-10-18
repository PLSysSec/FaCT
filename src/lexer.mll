{
open Parser
open Lexing

exception TokenError of string

let file : ((string option) ref) = ref None

let depth = ref 0

let raise_error lexbuf msg =
  let start = (lexeme_start lexbuf) - lexbuf.lex_curr_p.pos_bol in
  let ends = (lexeme_end lexbuf) - lexbuf.lex_curr_p.pos_bol in
  match lexbuf.lex_curr_p with
  | { pos_fname=fn; pos_lnum=pl; pos_bol=pb; pos_cnum=pc } ->
    match !file with
    | Some f ->
      raise (TokenError(msg ^ ", in file " ^ 
      f ^ ", line number " ^ (string_of_int pl) ^ ", position " ^ 
      (string_of_int start) ^ "-" ^ (string_of_int ends) ))
    | None -> raise (TokenError(msg ^ 
      ", at line number " ^ (string_of_int pl) ^ ", position " ^ 
      (string_of_int start) ^ "-" ^ (string_of_int ends)))

let raise_token_error lexbuf =
  let msg = "Unknown token, " ^ lexeme lexbuf ^ "`" in
  raise_error lexbuf msg

let raise_invalid_number lexbuf =
  let msg = "Invalid number, " ^ lexeme lexbuf ^ "`" in
  raise_error lexbuf msg

let keywords_table = Hashtbl.create 10
let add_keyword (k,v) = Hashtbl.add keywords_table k v

let keywords = [
  ("void",VOID);
  ("if",IF);
  ("else",ELSE);
  ("for",FOR);
  ("to",TO);
  ("return",RETURN);
  ("public",PUBLIC);
  ("secret",SECRET);
  ("const",CONST);
  ("mut",MUT);
  ("ref",REF);
  ("len",LEN);
  ("declassify",DECLASSIFY);
  ("arrzeros",ARRZEROS);
  ("arrcopy",ARRCOPY);
  ("arrview",ARRVIEW);
]
let _ = List.map add_keyword keywords
}

let whitespace = [' ' '\t' '\n']
let all_ints = ['0'-'9' 'A'-'F' 'a'-'f']
let ints = ['0'-'9']
let chars_ints = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let int_type = ("int" | "uint") ("8" | "16" | "32" | "64")?
let base_type = "bool" | int_type

rule token = parse
  | [' ' '\t']     { token lexbuf }
  | ";;"           { EX_END }
  | ":e"           { EX_START }
  | ":s"           { ST_START }
  | ":f"           { FD_START }
  | '\n'
  { Lexing.new_line lexbuf;
    token lexbuf }
  | "true"         { BOOL(true) }
  | "false"        { BOOL(false) }
  | base_type as c { TYPE(c) }
  | chars_ints * as c
    { try Hashtbl.find keywords_table c
      with Not_found -> IDENT c }
  | ints+ as lxm   { INT(int_of_string lxm) }
  | "0b"           { let buf = Buffer.create 10 in
                     Buffer.add_string buf "0b";
                     num buf lexbuf }
  | "0x"           { let buf = Buffer.create 10 in
                     Buffer.add_string buf "0x";
                     num buf lexbuf }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | "+="           { PLUSEQ }
  | "-="           { MINUSEQ }
  | "*="           { TIMESEQ }
  | "=="           { EQUAL }
  | "!="           { NEQUAL }
  | ">"            { GREATERTHAN }
  | "<"            { LESSTHAN }
  | ">="           { GREATERTHANEQ }
  | "<="           { LESSTHANEQ }
  | "!"            { LOGNOT }
  | "&&"           { LOGAND }
  | "||"           { LOGOR }
  | "&&="          { LOGANDEQ }
  | "||="          { LOGOREQ }
  | '&'            { BITAND }
  | '^'            { BITXOR }
  | '|'            { BITOR }
  | '~'            { BITNOT }
  | "&="           { BITANDEQ }
  | "^="           { BITXOREQ }
  | "|="           { BITOREQ }
  | "<<"           { LEFTSHIFT }
  | ">>"           { RIGHTSHIFT }
  | "<<="          { LEFTSHIFTEQ }
  | ">>="          { RIGHTSHIFTEQ }
  | "?"            { QUESTION }
  | ":"            { COLON }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | '['            { LBRACK }
  | ']'            { RBRACK }
  | "="            { ASSIGN }
  | ';'            { SEMICOLON }
  | ','            { COMMA }
  | "=>"           { RIGHTARROW }
  | "/*"           { depth := !depth + 1; commented lexbuf }
  | "//"           { ignore_line lexbuf }
  | eof            { EOF }
  | _              { raise_token_error lexbuf }

and commented = parse
  | "*/"  { depth := !depth - 1;
            if !depth = 0 then token lexbuf else commented lexbuf }
  | "/*"  { depth := !depth + 1;
            commented lexbuf }
  | '\n'  { Lexing.new_line lexbuf; commented lexbuf }
  | eof   { raise_error lexbuf "Comment is not closed" }
  | _     { commented lexbuf }

and ignore_line = parse
  | '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | _     { ignore_line lexbuf }

and num buf = parse
  | all_ints+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      let s = Buffer.contents buf in
      INT(int_of_string s)
    }
  | _      { raise_invalid_number lexbuf }
