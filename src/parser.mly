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

%token SECRET PUBLIC
%token REF OUT
%token RETURN
%token ZEROS UNSAFE_NOINIT
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

(* %nonassoc INT *)
(* %nonassoc RBRACK *)

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
  | var_type IDENT LPAREN fargs RPAREN LBRACE stmlist RBRACE
    { make_pos $startpos ({ name=$2; params=(List.rev $4); rvt=$1; body=$7 }) }
;

const_type:
  | TYPE { to_type $1 }

arg_labeled_type:
  | REF var_type
    { let vt = $2 in ltk vt Ref }
  | var_type LBRACK INT RBRACK
    { let vt = $1 in ltk vt (Arr $3) }
  | var_type
    { let vt = $1 in ltk vt Val }

var_type:
  | PUBLIC const_type
    { { v_ty=$2; v_lbl=Public } }
  | SECRET const_type
    { { v_ty=$2; v_lbl=Secret } }
  | const_type
    { { v_ty=$1; v_lbl=Unknown } }

fargs:
  | fargs COMMA arg_labeled_type IDENT
    { (make_pos $startpos { name=$4; lt=$3 })::$1}
  | arg_labeled_type IDENT
    { [make_pos $startpos { name=$2; lt=$1 }] }
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
;

arg:
  | expr
    { make_pos $startpos (ValArg $1) }
  | REF IDENT
    { make_pos $startpos (RefArg($2)) }
  | IDENT LBRACK RBRACK
    { make_pos $startpos (ArrArg($1)) }
    (* TODO: array slicing *)

arglist:
  | { [] }
  | arg { [$1] }
  | arg COMMA arglist { $1::$3 }

arrinit:
  | ZEROS { make_pos $startpos ZeroArray }
  | UNSAFE_NOINIT { make_pos $startpos UnsafeNoInit }

stmlist:
  | var_type IDENT ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (VarDec($2,$1,$4)))::$6 }
  | var_type LBRACK INT RBRACK IDENT ASSIGN arrinit SEMICOLON stmlist
    { (make_pos $startpos (ArrDec($5,$1,$3,$7)))::$9 }
  | IDENT LBRACK expr RBRACK ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (ArrAssign($1,$3,$6)))::$8 }
  | IDENT ASSIGN expr SEMICOLON stmlist
    { (make_pos $startpos (Assign($1,$3)))::$5 }
  | IDENT binopeq expr SEMICOLON stmlist
    { let makep = make_pos $startpos in
      let makee e = make_pos $startpos e in
      (makep (Assign($1,makee (BinOp($2,makee (VarExp($1)),$3)))))::$5 }
  | IF LPAREN expr RPAREN LBRACE stmlist RBRACE ELSE LBRACE stmlist RBRACE stmlist
    { (make_pos $startpos (If($3,$6,$10)))::$12 }
  | FOR LPAREN const_type IDENT ASSIGN expr TO expr RPAREN LBRACE stmlist RBRACE stmlist
    { (make_pos $startpos (For($4,$3,$6,$8,$11)))::$13 }
  | RETURN expr SEMICOLON stmlist
    { (make_pos $startpos (Return($2))::$4) }
  | { [] }

(* XXX unreachable
list_elements:
  | INT
    { [$1] }
  | INT COMMA list_elements
    { $1::$3 }
    *)

(* XXX unreachable
bytearr_list:
  | LBRACK list_elements RBRACK
    { make_pos $startpos (Primitive(ByteArray $2)) }
    *)

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
    { (make_pos $startpos L_And,$2) }
  | LOGOR expr
    { (make_pos $startpos L_Or,$2) }
  | BITOR expr
    { (make_pos $startpos B_Or,$2) }
  | BITXOR expr
    { (make_pos $startpos B_Xor,$2) }
  | BITAND expr
    { (make_pos $startpos B_And,$2) }
  | LEFTSHIFT expr
    { (make_pos $startpos LeftShift,$2) }
  | RIGHTSHIFT expr
    { (make_pos $startpos RightShift,$2) }
;

binopeq:
  | PLUSEQ { make_pos $startpos Plus }
  | MINUSEQ { make_pos $startpos Minus }
  | TIMESEQ { make_pos $startpos Multiply }
  | BITOREQ { make_pos $startpos B_Or }
  | BITXOREQ { make_pos $startpos B_Xor }
  | BITANDEQ { make_pos $startpos B_And }
  | LEFTSHIFTEQ { make_pos $startpos LeftShift }
  | RIGHTSHIFTEQ { make_pos $startpos RightShift }
;

unopexpr:
  | MINUS expr %prec UMINUS
    { make_pos $startpos (UnOp(make_pos $startpos Neg,$2)) }
  | LOGNOT expr
    { make_pos $startpos (UnOp(make_pos $startpos L_Not,$2)) }
  | BITNOT expr
    { make_pos $startpos (UnOp(make_pos $startpos B_Not,$2)) }
;

varexpr:
  | IDENT { make_pos $startpos (VarExp $1) }
;

arrexpr:
  | IDENT LBRACK expr RBRACK { make_pos $startpos (ArrExp($1,$3)) }
;

primitive:
  | INT
    { make_pos $startpos (Primitive(make_pos $startpos @@ Number $1)) }
  | BOOL
    { make_pos $startpos (Primitive(make_pos $startpos @@ Boolean $1)) }
;

callexp:
  | IDENT LPAREN arglist RPAREN
    { make_pos $startpos (CallExp($1,$3)) }
