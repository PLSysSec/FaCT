%{
open Ast
open Lexing

type ty_info = { ty:string; attr:int option }

(* TODO: I dont like this.. We need to think of another way
         to go from string to constantc type *)
let to_type = function
  | { ty="int"; attr=None } -> Int
  | { ty="bool"; attr=None } -> Bool
  | { ty="bytearr"; attr=(Some n) } -> ByteArr(n)

%}

%token <int> INT
%token <bool> BOOL
%token PLUS MINUS TIMES
%token EQUAL NEQUAL GREATERTHAN GREATERTHANEQ LESSTHAN LESSTHANEQ
%token BITOR BITAND LEFTSHIFT RIGHTSHIFT BITNOT
%token ASSIGN
%token LPAREN RPAREN

%token IF
%token <string> IDENT
%token FOR TO
%token LBRACK RBRACK

%token PRIVATE PUBLIC
%token RETURN
%token SEMICOLON
%token COMMA

%token EOF

%left EQUAL NEQUAL GREATERTHAN GREATERTHANEQ LESSTHAN LESSTHANEQ
%left PLUS MINUS
%left TIMES
%left BITOR BITAND LEFTSHIFT RIGHTSHIFT BITNOT

%start main
%type <Ast.fdec list> main
%type <Ast.expr> expr
%%
main:
  | fdeclist EOF { $1 }
;

fdeclist:
  | fdeclist fdec
    { $2 :: $1 }
  | fdec
    { [$1] }
;

fdec:
  | const_type IDENT LPAREN fargs RPAREN LBRACK stmlist RBRACK
    { FunctionDec($2,List.rev($4),$1,[$7],(to_pos $startpos)) }
;

const_type:
  | IDENT
    { let ty_info = { ty=$1; attr=None } in
      to_type ty_info }
  | IDENT LPAREN INT RPAREN
    { let ty_info = { ty=$1; attr=(Some $3) } in
      to_type ty_info }

fargs:
  | fargs COMMA const_type IDENT
    { {name=$4; ty=$3; p=(to_pos $startpos)}::$1}
  | const_type IDENT
    { [{name=($2); ty=$1; p=(to_pos $startpos)}] }
  | { [] }

expr:
  | INT
    { Primitive((Number $1),Some(to_pos $startpos)) }
  | BOOL
    { Primitive((Boolean $1),Some(to_pos $startpos)) }
  | LPAREN expr RPAREN
    { $2 }
  | expr binopexpr
    { let (b,e) = $2 in
      BinOp(b,$1,e,(to_pos $startpos)) }
  | unopexpr { $1 }
  | varexpr { $1 }
;

stmlist:
  | RETURN expr SEMICOLON
    { Return($2,(to_pos $startpos)) }

binopexpr:
  | PLUS expr
    { ((Plus(to_pos $startpos)),$2) }
  | MINUS expr
    { (Minus(to_pos $startpos),$2) }
  | TIMES expr
    { (Multiply(to_pos $startpos),$2) }
  | EQUAL expr
    { (Equal(to_pos $startpos),$2) }
  | NEQUAL expr
    { (NEqual(to_pos $startpos),$2) }
  | GREATERTHAN expr
    { (GT(to_pos $startpos),$2) }
  | GREATERTHANEQ expr
    { (GTE(to_pos $startpos),$2) }
  | LESSTHAN expr
    { (LT(to_pos $startpos),$2) }
  | LESSTHANEQ expr
    { (LTE(to_pos $startpos),$2) }
  | BITOR expr
    { (B_Or(to_pos $startpos),$2) }
  | BITAND expr
    { (B_And(to_pos $startpos),$2) }
  | LEFTSHIFT expr
    { (LeftShift(to_pos $startpos),$2) }
  | RIGHTSHIFT expr
    { (RightShift(to_pos $startpos),$2) }
;

unopexpr:
  | BITNOT expr
    { UnOp((B_Not (to_pos $startpos)),$2,(to_pos $startpos)) }
;

varexpr:
  | IDENT { VarExp($1,(to_pos $startpos)) }
;
