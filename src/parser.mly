%{
open Lexing
open Pos
open Ast

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout

exception ParseError of string

let to_type = function
  | "bool" -> Bool
  | "int8" -> Int 8
  | "int16" -> Int 16
  | "int32" -> Int 32
  | "uint8" -> UInt 8
  | "uint16" -> UInt 16
  | "uint32" -> UInt 32
  | _ as t -> raise (ParseError("Unknown type: " ^ t))
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
%token <string> TYPE
%token FOR TO
%token LBRACK RBRACK
%token LBRACE RBRACE

%token CONST MUT
%token SECRET PUBLIC
%token REF
%token RETURN
%token SEMICOLON COLON QUESTIONMARK
%token COMMA

%token EOF

(* precedence based on C operator precedence
 * http://en.cppreference.com/w/c/language/operator_precedence *)
%left QUESTIONMARK COLON
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
  | description IDENT LPAREN fargs RPAREN LBRACE stmlist RBRACE
    { make_pos $startpos ($2, (List.rev $4), $1, $7) }

base_type:
  | TYPE { to_type $1 }

ctype:
  | base_type LBRACK INT RBRACK
    { Arr($1, $3) }
  | base_type
    { Base $1 }

label:
  | PUBLIC { Public }
  | SECRET { Secret }
  | { Unknown }

mutability:
  | CONST { Const }
  | MUT { Mut }

description:
  | label mutability base_type
    { let lab = $1 in
      let mut = $2 in
      let ct = Base $3 in
      (ct, lab, mut) }

arg_description:
  | label mutability ctype
    { let lab = $1 in
      let mut = $2 in
      let ct = $3 in
      (ct, lab, mut) }
  | label ctype
    { let lab = $1 in
      let mut = Const in
      let ct = $2 in
      (ct, lab, mut) }

fargs:
  | fargs COMMA arg_description IDENT
    { (make_pos $startpos ($4, $3))::$1}
  | arg_description IDENT
    { [make_pos $startpos ($2, $1)] }
  | { [] }

expr:
  | LPAREN expr RPAREN
    { $2 }
  | expr binopexpr
    { let (b,e) = $2 in
      make_pos $startpos (BinOp(b,$1,e)) }
  | unopexpr { $1 }
  | varexpr { $1 }
  | arrexpr { $1 }
  | primitive { $1 }
  | callexp { $1 }
  | ternaryexp { $1 }
  | refexp { $1 }

arglist:
  | { [] }
  | expr { [$1] }
  | expr COMMA arglist { $1::$3 }

augmented_assignment:
  | IDENT binopeq expr SEMICOLON stmlist
    { let makep = make_pos $startpos in
      let makee e = make_pos $startpos e in
      (makep (VarAssign($1,makee (BinOp($2,makee (Var($1)),$3)))))::$5 }

stmlist:
  | description IDENT ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (VarDec($2,$1,$4)))::$6 }
  | IDENT LBRACK expr RBRACK ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (ArrAssign($1,$3,$6)))::$8 }
  | IDENT ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (VarAssign($1,$3)))::$5 }
  | augmented_assignment
    { $1 }
  | IF LPAREN expr RPAREN LBRACE stmlist RBRACE ELSE LBRACE stmlist RBRACE stmlist
    { (make_pos $startpos (If($3,$6,$10)))::$12 }
  | FOR LPAREN base_type IDENT ASSIGN expr TO expr RPAREN LBRACE stmlist RBRACE stmlist
    { (make_pos $startpos (For($3,$4,$6,$8,$11)))::$13 }
  | RETURN expr SEMICOLON stmlist
    { (make_pos $startpos (Return($2))::$4) }
  | { [] }

binopexpr:
  | PLUS expr
    { (make_pos $startpos Plus,$2) }
  | MINUS expr
    { (make_pos $startpos Minus,$2) }
  | TIMES expr
    { (make_pos $startpos Multiply,$2) }
  | EQUAL expr
    { (make_pos $startpos Equal,$2) }
  | NEQUAL expr
    { (make_pos $startpos NEqual,$2) }
  | GREATERTHAN expr
    { (make_pos $startpos GT,$2) }
  | GREATERTHANEQ expr
    { (make_pos $startpos GTE,$2) }
  | LESSTHAN expr
    { (make_pos $startpos LT,$2) }
  | LESSTHANEQ expr
    { (make_pos $startpos LTE,$2) }
  | LOGAND expr
    { (make_pos $startpos LogicalAnd,$2) }
  | LOGOR expr
    { (make_pos $startpos LogicalOr,$2) }
  | BITOR expr
    { (make_pos $startpos BitwiseOr,$2) }
  | BITXOR expr
    { (make_pos $startpos BitwiseXor,$2) }
  | BITAND expr
    { (make_pos $startpos BitwiseAnd,$2) }
  | LEFTSHIFT expr
    { (make_pos $startpos LeftShift,$2) }
  | RIGHTSHIFT expr
    { (make_pos $startpos RightShift,$2) }

binopeq:
  | PLUSEQ { make_pos $startpos Plus }
  | MINUSEQ { make_pos $startpos Minus }
  | TIMESEQ { make_pos $startpos Multiply }
  | BITOREQ { make_pos $startpos BitwiseOr }
  | BITXOREQ { make_pos $startpos BitwiseXor }
  | BITANDEQ { make_pos $startpos BitwiseAnd }
  | LEFTSHIFTEQ { make_pos $startpos LeftShift }
  | RIGHTSHIFTEQ { make_pos $startpos RightShift }

unopexpr:
  | MINUS expr %prec UMINUS
    { make_pos $startpos (UnOp(make_pos $startpos Negation,$2)) }
  | LOGNOT expr
    { make_pos $startpos (UnOp(make_pos $startpos LogicalNot,$2)) }
  | BITNOT expr
    { make_pos $startpos (UnOp(make_pos $startpos BitwiseNot,$2)) }

varexpr:
  | IDENT { make_pos $startpos (Var $1) }

arrexpr:
  | IDENT LBRACK expr RBRACK { make_pos $startpos (ArrAccess($1,$3)) }

primitive:
  | INT
    { make_pos $startpos (Number $1) }
  | BOOL
    { make_pos $startpos (Boolean $1) }

callexp:
  | IDENT LPAREN arglist RPAREN
    { make_pos $startpos (FunCall($1,$3)) }

ternaryexp:
  | expr QUESTIONMARK expr COLON expr
    { make_pos $startpos (TernaryOp($1, $3, $5)) }

refexp:
  | REF IDENT
    { make_pos $startpos (Ref $2) }