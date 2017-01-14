%{
open Ast
open Lexing

type ty_info = { ty:string; attr:expr option }

(* TODO: I dont like this.. We need to think of another way
         to go from string to constantc type *)
let to_type = function
  | { ty="int"; attr=None } -> Int
  | { ty="bool"; attr=None } -> Bool
  | { ty="bytearr"; attr=(Some (Primitive((Number n),_))) } -> ByteArr n

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout

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

%token PRIVATE PUBLIC
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
  | const_type IDENT LPAREN fargs RPAREN LBRACE stmlist RBRACE
    { FunctionDec($2,List.rev($4),$1,$7,(to_pos $startpos)) }
;

const_type:
  | IDENT
    { let ty_info = { ty=$1; attr=None } in
      to_type ty_info }
  | IDENT LBRACK expr RBRACK
    { let ty_info = { ty=$1; attr=(Some $3) } in
      to_type ty_info }

labeled_type:
  | PUBLIC const_type
    { Public $2 }
  | PRIVATE const_type
    { Private $2}

fargs:
  | fargs COMMA labeled_type IDENT
    { {name=$4; ty=$3; p=(to_pos $startpos)}::$1}
  | labeled_type IDENT
    { [{name=($2); ty=$1; p=(to_pos $startpos)}] }
  | { [] }

expr:
  | LPAREN expr RPAREN
    { $2 }
  | expr binopexpr
    { let (b,e) = $2 in
      BinOp(b,$1,e,(to_pos $startpos)) }
  | unopexpr { $1 }
  | varexpr { $1 }
  | arrexpr { $1 }
  | primitive { $1 }
  | callexp { $1 }
;

exprlist:
  | { [] }
  | expr { [$1] }
  | expr COMMA exprlist { $1::$3 }

stmlist:
  | IDENT LBRACK expr RBRACK ASSIGN expr SEMICOLON stmlist
    { (ArrAssign($1,$3,$6,(to_pos $startpos)))::$8 }
  | labeled_type IDENT ASSIGN expr SEMICOLON stmlist
    { (VarDec($2,$1,$4,(to_pos $startpos)))::$6 }
  | IDENT ASSIGN expr SEMICOLON stmlist
    { (Assign($1,$3,(to_pos $startpos)))::$5 }
  | IDENT binopeq expr SEMICOLON stmlist
    { let p = (to_pos $startpos) in
      (Assign($1,BinOp($2,VarExp($1,p),$3,p),(to_pos $startpos)))::$5 }
  | IF LPAREN expr RPAREN LBRACE stmlist RBRACE ELSE LBRACE stmlist RBRACE stmlist
    { (If($3,$6,$10,(to_pos $startpos)))::$12 }
  | FOR LPAREN IDENT ASSIGN primitive_not_expr TO primitive_not_expr RPAREN LBRACE stmlist RBRACE stmlist
    { (For($3,$5,$7,$10,(to_pos $startpos)))::$12 }
  | RETURN expr SEMICOLON stmlist
    { (Return($2,(to_pos $startpos))::$4) }
  | { [] }

list_elements:
  | INT
    { [$1] }
  | INT COMMA list_elements
    { $1::$3 }

bytearr_list:
  | LBRACK list_elements RBRACK
    { Primitive(ByteArray $2, Some(to_pos $startpos)) }

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
  | LOGAND expr
    { (L_And(to_pos $startpos),$2) }
  | LOGOR expr
    { (L_Or(to_pos $startpos),$2) }
  | BITOR expr
    { (B_Or(to_pos $startpos),$2) }
  | BITXOR expr
    { (B_Xor(to_pos $startpos),$2) }
  | BITAND expr
    { (B_And(to_pos $startpos),$2) }
  | LEFTSHIFT expr
    { (LeftShift(to_pos $startpos),$2) }
  | RIGHTSHIFT expr
    { (RightShift(to_pos $startpos),$2) }
;

binopeq:
  | PLUSEQ { Plus(to_pos $startpos) }
  | MINUSEQ { Minus(to_pos $startpos) }
  | TIMESEQ { Multiply(to_pos $startpos) }
  | BITOREQ { B_Or(to_pos $startpos) }
  | BITXOREQ { B_Xor(to_pos $startpos) }
  | BITANDEQ { B_And(to_pos $startpos) }
  | LEFTSHIFTEQ { LeftShift(to_pos $startpos) }
  | RIGHTSHIFTEQ { RightShift(to_pos $startpos) }
;

unopexpr:
  | MINUS expr %prec UMINUS
    { UnOp((Neg (to_pos $startpos)),$2,(to_pos $startpos)) }
  | LOGNOT expr
    { UnOp((L_Not (to_pos $startpos)),$2,(to_pos $startpos)) }
  | BITNOT expr
    { UnOp((B_Not (to_pos $startpos)),$2,(to_pos $startpos)) }
;

varexpr:
  | IDENT { VarExp($1,(to_pos $startpos)) }
;

arrexpr:
  | IDENT LBRACK expr RBRACK { ArrExp($1,$3,(to_pos $startpos)) }
;

primitive:
  | INT
    { Primitive((Number $1),Some(to_pos $startpos)) }
  | BOOL
    { Primitive((Boolean $1),Some(to_pos $startpos)) }
;

primitive_not_expr:
  | INT { Number $1 }
  | BOOL { Boolean $1 }
;

callexp:
  | IDENT LPAREN exprlist RPAREN
    { CallExp($1,$3,(to_pos $startpos)) }
