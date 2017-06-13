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
%token VOID
%token PLUS MINUS TIMES
%token EQUAL NEQUAL GREATERTHAN GREATERTHANEQ LESSTHAN LESSTHANEQ
%token LOGNOT LOGAND LOGOR
%token BITOR BITXOR BITAND LEFTSHIFT RIGHTSHIFT BITNOT
%token ASSIGN
%token PLUSEQ MINUSEQ TIMESEQ
%token BITOREQ BITXOREQ BITANDEQ LEFTSHIFTEQ RIGHTSHIFTEQ
%token QUESTION COLON
%token LPAREN RPAREN

%token IF ELSE
%token <string> IDENT
%token <string> TYPE
%token FOR TO
%token LBRACK RBRACK
%token LBRACE RBRACE

%token SECRET PUBLIC
%token CONST MUT
%token REF
%token RETURN
%token DECLASSIFY
%token UNSAFE_NOINIT
%token ARRZEROS ARRCOPY ARRVIEW
%token SEMICOLON
%token COMMA
%token LEN

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

(* Preprocessor shenanigans *)
#define mkpos make_pos $symbolstartpos $endpos
#define mkposof(x) make_pos $startpos(x) $endpos(x)

%start <Ast.statement list> main
%%
main:
  | list(statement) EOF { $1 }

%inline paren(X):
  | LPAREN x=X RPAREN { x }

%inline brack(X):
  | LBRACK x=X RBRACK { x }

%inline plist(X):
  | LPAREN xs=separated_list(COMMA, X) RPAREN { xs }

base_type:
  | TYPE { mkpos (to_type $1) }

label:
  | PUBLIC { mkpos Public }
  | SECRET { mkpos Secret }

mutability:
  | CONST { mkpos Const }
  | MUT { mkpos Mut }

unop:
  | MINUS %prec UMINUS { Neg }
  | LOGNOT { LogicalNot }
  | BITNOT { BitwiseNot }

%inline binop:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Multiply }
  | EQUAL { Equal }
  | NEQUAL { NEqual }
  | GREATERTHAN { GT }
  | GREATERTHANEQ { GTE }
  | LESSTHAN { LT }
  | LESSTHANEQ { LTE }
  | LOGAND { LogicalAnd }
  | LOGOR { LogicalOr }
  | BITOR { BitwiseOr }
  | BITXOR { BitwiseXor }
  | BITAND { BitwiseAnd }
  | LEFTSHIFT { LeftShift }
  | RIGHTSHIFT { RightShift }

arg:
  | e=expr { mkpos (ByValue e) }
  | a=array_expr { mkpos (ByArray a) }
  | REF x=IDENT { mkpos (ByRef x) }

expr:
  | e=paren(expr) { mkpos e.data }
  | BOOL { mkpos (if $1 then True else False) }
  | INT { mkpos (IntLiteral $1) }
  | IDENT { mkpos (Variable $1) }
  | a=IDENT e=brack(expr) { mkpos (ArrayGet(a, e)) }
  | LEN a=IDENT { mkpos (ArrayLen a) }
  | b=paren(base_type) e=expr { mkpos (IntCast(b, e)) }
  | op=unop e=expr { mkpos (UnOp(op, e)) }
  | e1=expr op=binop e2=expr { mkpos (BinOp(op, e1, e2)) }
  | e1=expr QUESTION e2=expr COLON e3=expr { mkpos (TernOp(e1, e2, e3)) }
  | fn=IDENT args=plist(arg) { mkpos (FnCall(fn, args)) }
  | DECLASSIFY e=paren(expr) { mkpos (Declassify e) }

lexpr:
  | INT { mkpos (LIntLiteral $1) }
  (* ... *)

array_expr:
  | ARRZEROS l=lexpr { mkpos (ArrayZeros l) }
  (* ... *)

base_variable_type:
  | b=base_type
    { mkpos (RefVT(mkpos (Ref b), mkpos Unknown, mkpos Const)) }
  | l=label b=base_type
    { mkpos (RefVT({b with data=Ref b}, l, mkpos Const)) }
  | m=mutability b=base_type
    { mkpos (RefVT({b with data=Ref b}, mkpos Unknown, m)) }
  | l=label m=mutability b=base_type
    { mkpos (RefVT({b with data=Ref b}, l, m)) }

statement:
  (* ... *)
  | x=IDENT ASSIGN e=expr SEMICOLON { mkpos (BaseAssign(x, e)) }
  (* ... *)

(*function_decs:
  | function_dec function_decs
    { $1 :: $2 }
  | function_dec
    { [$1] }

function_dec:
  | ret_type IDENT LPAREN args RPAREN LBRACE body RBRACE
    { make_pos $symbolstartpos (FunDec($2, $1, $4, $7)) }

ret_type:
  | VOID { None }
  | label base_type
    { Some (make_pos $symbolstartpos (BaseET($2, $1))) }

args:
  | arg COMMA args
    { $1 :: $3 }
  | arg
    { [$1] }
  | { [] }

arg:
  | arg_type IDENT
    { make_pos $symbolstartpos (Arg($2, $1)) }

arg_type:
  | label base_type
    { let makep = make_pos $symbolstartpos in
      makep (RefVT(makep (Ref $2), $1, makep Const)) }
  | label mutability base_type
    { let makep = make_pos $symbolstartpos in
      makep (RefVT(makep (Ref $3), $1, $2)) }
  | label array_type
    { let makep = make_pos $symbolstartpos in
      makep (ArrayET($2, $1, makep Const)) }
  | label mutability array_type
    { let makep = make_pos $symbolstartpos in
      makep (ArrayET($3, $1, $2)) }

array_type:
  | base_type LBRACK lexpr RBRACK
    { make_pos $symbolstartpos (ArrayAT($1, $3)) }

lexpr:
  | INT { make_pos $symbolstartpos (LIntLiteral $1) }
  | IDENT { make_pos $symbolstartpos (LVariable $1) }
  | LEN IDENT { make_post $symbolstartpos (LLength $2) }

body:
  | statements { $1 }

statements:
  | statement statements
    { $1 :: $2 }
  | statement
    { [$1] }
  | { [] }

statement:
  | base_variable_type IDENT ASSIGN expr
    { make_pos $symbolstartpos (BaseDec($2, $1, $4)) }
  | array_variable_type IDENT ASSIGN array_expr
    { make_pos $symbolstartpos (ArrayDec($2, $1, $4)) }
  | IDENT ASSIGN expr
    { make_pos $symbolstartpos (BaseAssign($1, $3)) }
  | IDENT LBRACK expr RBRACK ASSIGN expr
    { make_pos $symbolstartpos (ArrayAssign($1, $3, $6)) }
  | IF LPAREN expr RPAREN LBRACE statements RBRACE
    { make_pos $symbolstartpos (If($3, $6, [])) }
  | IF LPAREN e=expr RPAREN LBRACE thenstms=statements RBRACE
      list(ELSE IF LPAREN expr RPAREN LBRACE statements RBRACE { })
      option(ELSE LBRACE statements RBRACE { })
    { let makep = makepos $symbolstartpos in
      makep (If(e, thenstms, [])) }
  | IF LPAREN expr RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE
    { make_pos $symbolstartpos (If($3, $6, $10)) }
  | IF LPAREN expr RPAREN LBRACE statements RBRACE ELSE IF LBRACE statements RBRACE

array_variable_type:
  | array_type
    { let makep = make_pos $symbolstartpos in
      makep (ArrayVT($1, makep Unknown, makep Const)) }
  | label array_type
    { let makep = make_pos $symbolstartpos in
      makep (ArrayVT($2, $1, makep Const)) }
  | mutability array_type
    { let makep = make_pos $symbolstartpos in
      makep (ArrayVT($2, makep Unknown, $1)) }
  | label mutability array_type
    { let makep = make_pos $symbolstartpos in
      makep (ArrayVT($3, $1, $2)) }

*)(* ----

const_type:
  | TYPE { make_pos $symbolstartpos (to_type $1) }

arg_labeled_type:
  | REF var_type
    { let vt = $2 in ltk vt Ref }
  | var_type LBRACK INT RBRACK
    { let vt = $1 in ltk vt (Arr $3) }
  | var_type LBRACK IDENT RBRACK
    { let vt = $1 in ltk vt (DArr $3) }
  | var_type
    { let vt = $1 in ltk vt Val }

var_type:
  | PUBLIC const_type
    { { v_ty=$2; v_lbl=Public } }
  | SECRET const_type
    { { v_ty=$2; v_lbl=Secret } }
  | const_type
    { { v_ty=$1; v_lbl=Unknown } }

expr:
  | LPAREN expr RPAREN
    { $2 }
  | expr binopexpr
    { let (b,e) = $2 in
      make_pos $symbolstartpos (BinOp(b,$1,e)) }
  | unopexpr { $1 }
  | varexpr { $1 }
  | arrexpr { $1 }
  | primitive { $1 }
  | callexp { $1 }
;

arg:
  | expr
    { make_pos $symbolstartpos (ValArg $1) }
  | REF IDENT
    { make_pos $symbolstartpos (RefArg($2)) }
  | IDENT LBRACK RBRACK
    { make_pos $symbolstartpos (ArrArg($1)) }
    (* TODO: array slicing *)

arglist:
  | { [] }
  | arg { [$1] }
  | arg COMMA arglist { $1::$3 }

arrinit:
  | UNSAFE_NOINIT { make_pos $symbolstartpos UnsafeNoInit }

stmlist:
  | var_type IDENT ASSIGN expr SEMICOLON stmlist
    { (make_pos $symbolstartpos (VarDec($2,$1,$4)))::$6 }
  | var_type LBRACK INT RBRACK IDENT ASSIGN arrinit SEMICOLON stmlist
    { (make_pos $symbolstartpos (ArrDec($5,$1,$3,$7)))::$9 }
  | IDENT LBRACK expr RBRACK ASSIGN expr SEMICOLON stmlist
    { (make_pos $symbolstartpos (ArrAssign($1,$3,$6)))::$8 }
  | IDENT ASSIGN expr SEMICOLON stmlist
    { (make_pos $symbolstartpos (Assign($1,$3)))::$5 }
  | IDENT binopeq expr SEMICOLON stmlist
    { let makep = make_pos $symbolstartpos in
      let makee e = make_pos $symbolstartpos e in
      (makep (Assign($1,makee (BinOp($2,makee (VarExp($1)),$3)))))::$5 }
  | IF LPAREN expr RPAREN LBRACE stmlist RBRACE ELSE LBRACE stmlist RBRACE stmlist
    { (make_pos $symbolstartpos (If($3,$6,$10)))::$12 }
  | FOR LPAREN const_type IDENT ASSIGN expr TO expr RPAREN LBRACE stmlist RBRACE stmlist
    { (make_pos $symbolstartpos (For($4,$3,$6,$8,$11)))::$13 }
  | RETURN expr SEMICOLON stmlist
    { (make_pos $symbolstartpos (Return($2))::$4) }
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
    { make_pos $symbolstartpos (Primitive(ByteArray $2)) }
    *)

binopexpr:
  | PLUS expr
    { (make_pos $symbolstartpos Plus,$2) }
  | MINUS expr
    { (make_pos $symbolstartpos Minus,$2) }
  | TIMES expr
    { (make_pos $symbolstartpos Multiply,$2) }
  | EQUAL expr
    { (make_pos $symbolstartpos Equal,$2) }
  | NEQUAL expr
    { (make_pos $symbolstartpos NEqual,$2) }
  | GREATERTHAN expr
    { (make_pos $symbolstartpos GT,$2) }
  | GREATERTHANEQ expr
    { (make_pos $symbolstartpos GTE,$2) }
  | LESSTHAN expr
    { (make_pos $symbolstartpos LT,$2) }
  | LESSTHANEQ expr
    { (make_pos $symbolstartpos LTE,$2) }
  | LOGAND expr
    { (make_pos $symbolstartpos L_And,$2) }
  | LOGOR expr
    { (make_pos $symbolstartpos L_Or,$2) }
  | BITOR expr
    { (make_pos $symbolstartpos B_Or,$2) }
  | BITXOR expr
    { (make_pos $symbolstartpos B_Xor,$2) }
  | BITAND expr
    { (make_pos $symbolstartpos B_And,$2) }
  | LEFTSHIFT expr
    { (make_pos $symbolstartpos LeftShift,$2) }
  | RIGHTSHIFT expr
    { (make_pos $symbolstartpos RightShift,$2) }
;

binopeq:
  | PLUSEQ { make_pos $symbolstartpos Plus }
  | MINUSEQ { make_pos $symbolstartpos Minus }
  | TIMESEQ { make_pos $symbolstartpos Multiply }
  | BITOREQ { make_pos $symbolstartpos B_Or }
  | BITXOREQ { make_pos $symbolstartpos B_Xor }
  | BITANDEQ { make_pos $symbolstartpos B_And }
  | LEFTSHIFTEQ { make_pos $symbolstartpos LeftShift }
  | RIGHTSHIFTEQ { make_pos $symbolstartpos RightShift }
;

unopexpr:
  | MINUS expr %prec UMINUS
    { make_pos $symbolstartpos (UnOp(make_pos $symbolstartpos Neg,$2)) }
  | LOGNOT expr
    { make_pos $symbolstartpos (UnOp(make_pos $symbolstartpos L_Not,$2)) }
  | BITNOT expr
    { make_pos $symbolstartpos (UnOp(make_pos $symbolstartpos B_Not,$2)) }
;

varexpr:
  | IDENT { make_pos $symbolstartpos (Variable $1) }
;

arrexpr:
  | IDENT LBRACK expr RBRACK { make_pos $symbolstartpos (ArrExp($1,$3)) }
;

primitive:
  | INT
    { make_pos $symbolstartpos (Primitive(make_pos $symbolstartpos @@ Number $1)) }
  | BOOL
    { make_pos $symbolstartpos (Primitive(make_pos $symbolstartpos @@ Boolean $1)) }
;

callexp:
  | IDENT LPAREN arglist RPAREN
    { make_pos $symbolstartpos (CallExp($1,$3)) }
*)
