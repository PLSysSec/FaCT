{
open Parser
open Lexing

exception TokenError of string

let file : ((string option) ref) = ref None

let raise_token_error lexbuf =
  let start = (lexeme_start lexbuf) - lexbuf.lex_curr_p.pos_bol in
  let ends = (lexeme_end lexbuf) - lexbuf.lex_curr_p.pos_bol in
  match lexbuf.lex_curr_p with
  | { pos_fname=fn; pos_lnum=pl; pos_bol=pb; pos_cnum=pc } ->
    match !file with
    | Some f ->
      raise (TokenError("Unknown token, `" ^ lexeme lexbuf ^ "`, in file " ^ 
      f ^ ", line number " ^ (string_of_int pl) ^ ", position " ^ 
      (string_of_int start) ^ "-" ^ (string_of_int ends) ))
    | None -> raise (TokenError("Unknown token, `" ^ lexeme lexbuf ^ 
      "`, at line number " ^ (string_of_int pl) ^ ", position " ^ 
      (string_of_int start) ^ "-" ^ (string_of_int ends)))

let keywords_table = Hashtbl.create 10
let add_keyword (k,v) = Hashtbl.add keywords_table k v

(* TODO: This is incomplete *)
let keywords = [
  ("if",IF);
  ("else",ELSE);
  ("for",FOR);
  ("to",TO);
  ("return",RETURN);
]
let _ = List.map add_keyword keywords
}

let chars = ['A'-'Z' 'a'-'z' '_' '\'']
let ints = ['0'-'9']

rule token = parse
  | [' ' '\t']     { token lexbuf }
  | '\n'
  { new_line lexbuf;
  token lexbuf }
  | "true"         { BOOL(true)}
  | "false"        { BOOL(false)}
  | chars * as c
    { try Hashtbl.find keywords_table c
      with Not_found -> IDENT c}
  | ints+ as lxm   { INT(int_of_string lxm) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '='            { EQUAL }
  | ":="           { ASSIGN }
  | "!="           { NEQUAL }
  | ">"            { GREATERTHAN }
  | "<"            { LESSTHAN }
  | ">="           { GREATERTHANEQ }
  | "<="           { LESSTHANEQ }
  | "&"            { BITAND }
  | "|"            { BITOR }
  | "<<"           { LEFTSHIFT }
  | ">>"           { RIGHTSHIFT }
  | "~"            { BITNOT }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | '['            { LBRACK }
  | ']'            { RBRACK }
  | ';'            { SEMICOLON }
  | ','            { COMMA }
  | eof            {
    let l = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <- { l with
        pos_fname="vgershlegr";
        }; EOF
      }
  | _              { raise_token_error lexbuf }
