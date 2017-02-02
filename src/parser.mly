%{
open Ast
open Lexing

type ty_info = { ty:string; attr:int option }

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout

exception ParseError of string

(* TODO: I dont like this.. We need to think of another way
         to go from string to constantc type *)
let to_type = function
  | { ty="int"; attr=None } -> Int 32
  | { ty="int32"; attr=None } -> Int 32
  | { ty="int16"; attr=None } -> Int 16
  | { ty="int8"; attr=None } -> Int 8
  | { ty="uint32"; attr=None } -> UInt 32
  | { ty="uint16"; attr=None } -> UInt 16
  | { ty="uint8"; attr=None } -> UInt 8
  | { ty="bool"; attr=None } -> Bool
  | { ty="int"; attr=(Some n) } ->
    Array { size=n; ty=Int 32 }
  | { ty="int32"; attr=(Some n) } ->
    Array { size=n; ty=Int 32 }
  | { ty="int16"; attr=(Some n) } ->
    Array { size=n; ty=Int 16 }
  | { ty="int8"; attr=(Some n) } ->
    Array { size=n; ty=Int 8 }
  | { ty="uint32"; attr=(Some n) } ->
    Array { size=n; ty=UInt 32 }
  | { ty="uint16"; attr=(Some n) } ->
    Array { size=n; ty=UInt 16 }
  | { ty="uint8"; attr=(Some n) } ->
    Array { size=n; ty=UInt 8 }
  | { ty="bool"; attr=(Some n) } ->
    Array { size=n; ty=Bool }
  | { ty=t } -> raise (ParseError("Unknown type: " ^ t))

let make_exp p e = make_pos p { e=e; ty=None; label=None }

%}

%token <int> INT
%token <bool> BOOL
%token PLUS MINUS TIMES
%token EQUAL NEQUAL GREATERTHAN GREATERTHANEQ LESSTHAN LESSTHANEQ
%token LOGNOT LOGAND LOGOR
%token BITOR BITXOR BITAND LEFTSHIFT RIGHTSHIFT BITNOT
%token ASSIGN
%token PLUSEQ MINUSEQ TIMESEQ
%token BITOREQ BITXOREQ BITANDEQ LEFTSHIFTEQ RIGHTSHIFTEQ
%token LPAREN RPAREN

%token IF ELSE
%token <string> IDENT
%token FOR TO
%token LBRACK RBRACK
%token LBRACE RBRACE

%token SECRET PUBLIC
%token REF OUT
%token RETURN
%token SEMICOLON
%token COMMA

%token EOF

(* precedence based on C operator precedence
 * http://en.cppreference.com/w/c/language/operator_precedence *)
%left LOGOR
%left LOGAND
%left BITOR
%left BITXOR
%left BITAND
%left EQUAL NEQUAL
%left GREATERTHAN GREATERTHANEQ LESSTHAN LESSTHANEQ
%left LEFTSHIFT RIGHTSHIFT
%left PLUS MINUS
%left TIMES
%left LOGNOT BITNOT UMINUS

%nonassoc INT
%nonassoc RBRACK

%start main
%type <Ast.fdec list> main
%type <Ast.expr> expr
%%
main:
  | fdeclist EOF { List.rev($1) }
;

fdeclist:
  | fdeclist fdec
    { $2 :: $1 }
  | fdec
    { [$1] }
;

fdec:
  | expr_type IDENT LPAREN fargs RPAREN LBRACE stmlist RBRACE
    { make_pos $startpos ({ name=$2; params=(List.rev $4); rty=$1; body=$7 }) }
;

const_type:
  | IDENT
    { let ty_info = { ty=$1; attr=None } in
      to_type ty_info }
  | IDENT LBRACK INT RBRACK
    { let ty_info = { ty=$1; attr=(Some $3) } in
      to_type ty_info }

arg_labeled_type:
  | OUT labeled_type
    { let lt = $2 in
      { lt with kind=Out } }
  | REF labeled_type
    { let lt = $2 in
      { lt with kind=Ref } }
  | labeled_type
    { let lt = $1 in
      { lt with kind=Val } }

expr_type:
  | PUBLIC const_type
    { { ty=$2; label=Some Public } }
  | SECRET const_type
    { { ty=$2; label=Some Secret } }
  | const_type
    { { ty=$1; label=None } }

labeled_type:
  | PUBLIC const_type
    { { ty=$2; label=Some Public; kind=Val } }
  | SECRET const_type
    { { ty=$2; label=Some Secret; kind=Val } }
  | const_type
    { { ty=$1; label=None; kind=Val } }

fargs:
  | fargs COMMA arg_labeled_type IDENT
    { {name=$4; lt=$3}::$1}
  | arg_labeled_type IDENT
    { [{name=($2); lt=$1}] }
  | { [] }

expr:
  | LPAREN expr RPAREN
    { $2 }
  | expr binopexpr
    { let (b,e) = $2 in
      make_exp $startpos (BinOp(b,$1,e)) }
  | unopexpr { $1 }
  | varexpr { $1 }
  | arrexpr { $1 }
  | primitive { $1 }
  | callexp { $1 }
;

arg:
  | expr
    { make_pos $startpos (ValArg $1) }
  | OUT IDENT
    { make_pos $startpos (VarArg(Out,$2)) }
  | REF IDENT
    { make_pos $startpos (VarArg(Ref,$2)) }
    (* TODO: array slicing *)

arglist:
  | { [] }
  | arg { [$1] }
  | arg COMMA arglist { $1::$3 }

stmlist:
  | IDENT LBRACK expr RBRACK ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (ArrAssign($1,$3,$6)))::$8 }
  | labeled_type IDENT ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (VarDec($2,$1,$4)))::$6 }
  | IDENT ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (Assign($1,$3)))::$5 }
  | IDENT binopeq expr SEMICOLON stmlist
    { let makep = make_pos $startpos in
      let makee e = make_exp $startpos e in
      (makep (Assign($1,makee (BinOp($2,makee (VarExp($1)),$3)))))::$5 }
  | IF LPAREN expr RPAREN LBRACE stmlist RBRACE ELSE LBRACE stmlist RBRACE stmlist
    { (make_pos $startpos (If($3,$6,$10)))::$12 }
  | FOR LPAREN const_type IDENT ASSIGN expr TO expr RPAREN LBRACE stmlist RBRACE stmlist
    { (make_pos $startpos (For($4,$3,$6,$8,$11)))::$13 }
  | RETURN expr SEMICOLON stmlist
    { (make_pos $startpos (Return($2))::$4) }
  | { [] }

list_elements:
  | INT
    { [$1] }
  | INT COMMA list_elements
    { $1::$3 }

bytearr_list:
  | LBRACK list_elements RBRACK
    { make_pos $startpos (Primitive(ByteArray $2)) }

binopexpr:
  | PLUS expr
    { (Plus,$2) }
  | MINUS expr
    { (Minus,$2) }
  | TIMES expr
    { (Multiply,$2) }
  | EQUAL expr
    { (Equal,$2) }
  | NEQUAL expr
    { (NEqual,$2) }
  | GREATERTHAN expr
    { (GT,$2) }
  | GREATERTHANEQ expr
    { (GTE,$2) }
  | LESSTHAN expr
    { (LT,$2) }
  | LESSTHANEQ expr
    { (LTE,$2) }
  | LOGAND expr
    { (L_And,$2) }
  | LOGOR expr
    { (L_Or,$2) }
  | BITOR expr
    { (B_Or,$2) }
  | BITXOR expr
    { (B_Xor,$2) }
  | BITAND expr
    { (B_And,$2) }
  | LEFTSHIFT expr
    { (LeftShift,$2) }
  | RIGHTSHIFT expr
    { (RightShift,$2) }
;

binopeq:
  | PLUSEQ { Plus }
  | MINUSEQ { Minus }
  | TIMESEQ { Multiply }
  | BITOREQ { B_Or }
  | BITXOREQ { B_Xor }
  | BITANDEQ { B_And }
  | LEFTSHIFTEQ { LeftShift }
  | RIGHTSHIFTEQ { RightShift }
;

unopexpr:
  | MINUS expr %prec UMINUS
    { make_exp $startpos (UnOp(Neg,$2)) }
  | LOGNOT expr
    { make_exp $startpos (UnOp(L_Not,$2)) }
  | BITNOT expr
    { make_exp $startpos (UnOp(B_Not,$2)) }
;

varexpr:
  | IDENT { make_exp $startpos (VarExp $1) }
;

arrexpr:
  | IDENT LBRACK expr RBRACK { make_exp $startpos (ArrExp($1,$3)) }
;

primitive:
  | INT
    { make_exp $startpos (Primitive(Number $1)) }
  | BOOL
    { make_exp $startpos (Primitive(Boolean $1)) }
;

callexp:
  | IDENT LPAREN arglist RPAREN
    { make_exp $startpos (CallExp($1,$3)) }
