%{
open Lexing
open Pos
open Err
open Ast

let to_type { data=t; pos=p } =
  match t with
    | "bool" -> Bool
    | "int8" -> Int 8
    | "int16" -> Int 16
    | "int32" -> Int 32
    | "int64" -> Int 64
    | "uint8" -> UInt 8
    | "uint16" -> UInt 16
    | "uint32" -> UInt 32
    | "uint64" -> UInt 64
    | "uint128" -> UInt 128
    | _ as t -> raise (errParseType p t)
%}

%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token VOID
%token PLUS MINUS TIMES DIVIDE MODULO
%token EQUAL NEQUAL GREATERTHAN GREATERTHANEQ LESSTHAN LESSTHANEQ
%token LOGNOT LOGAND LOGOR
%token BITOR BITXOR BITAND LEFTSHIFT LEFTROTATE RIGHTSHIFT RIGHTROTATE BITNOT
%token ASSIGN
%token PLUSEQ MINUSEQ TIMESEQ DIVIDEEQ MODULOEQ
%token LOGANDEQ LOGOREQ
%token BITOREQ BITXOREQ BITANDEQ LEFTSHIFTEQ LEFTROTATEEQ RIGHTSHIFTEQ RIGHTROTATEEQ
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
%token ARRZEROS ARRCOPY ARRVIEW NOINIT
%token SEMICOLON
%token COMMA
%token LEN RIGHTARROW
%token EXTERN INLINE EXPORT NOINLINE
%token STRUCT EMBED DOT

%token FD_START ST_START EX_START EX_END

%token EOF

(* precedence based on C operator precedence
 * http://en.cppreference.com/w/c/language/operator_precedence *)
%left QUESTION
%left LOGOR
%left LOGAND
%left BITOR
%left BITXOR
%left BITAND
%left EQUAL NEQUAL
%left GREATERTHAN GREATERTHANEQ LESSTHAN LESSTHANEQ
%left LEFTSHIFT RIGHTSHIFT LEFTROTATE RIGHTROTATE
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left DOT
%nonassoc UNARYOP

(* Preprocessor shenanigans *)
#define mkpos make_pos $symbolstartpos $endpos
#define mkposof(x) make_pos $startpos(x) $endpos(x)
#define mkposrange(x,y) make_pos $startpos(x) $endpos(y)

%start <Ast.fact_module> main
%start <Ast.top_level> top_level

%%
main:
  | fact_module EOF { $1 }

%inline paren(X):
  | LPAREN x=X RPAREN { x }

%inline brack(X):
  | LBRACK x=X RBRACK { x }

%inline plist(X):
  | LPAREN xs=separated_list(COMMA, X) RPAREN { xs }

%inline blist(X):
  | LBRACE xs=list(X) RBRACE { xs }

%inline alist(X):
  | LBRACE xs=separated_list(COMMA, X) RBRACE { xs }

var_name:
  | x=IDENT { mkpos x }

fun_name:
  | f=IDENT { mkpos f }

struct_name:
  | s=IDENT { mkpos s }

base_type:
  | t=TYPE { mkpos (to_type (mkpos t)) }

array_type:
  | b=base_type l=brack(lexpr) { mkpos (ArrayAT(b, l)) }
  | b=base_type LBRACK RBRACK { mkpos (ArrayAT(b, mkpos LUnspecified)) }

label:
  | PUBLIC { mkpos Public }
  | SECRET { mkpos Secret }

mutability:
  | CONST { mkpos Const }
  | MUT { mkpos Mut }

unop:
  | MINUS { Neg }
  | LOGNOT { LogicalNot }
  | BITNOT { BitwiseNot }

%inline binop:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Multiply }
  | DIVIDE { Divide }
  | MODULO { Modulo }
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
  | LEFTROTATE { LeftRotate }
  | RIGHTROTATE { RightRotate }

%inline binopeq:
  | PLUSEQ { Plus }
  | MINUSEQ { Minus }
  | TIMESEQ { Multiply }
  | DIVIDEEQ { Divide }
  | MODULOEQ { Modulo }
  | LOGANDEQ { LogicalAnd }
  | LOGOREQ { LogicalOr }
  | BITOREQ { BitwiseOr }
  | BITXOREQ { BitwiseXor }
  | BITANDEQ { BitwiseAnd }
  | LEFTSHIFTEQ { LeftShift }
  | RIGHTSHIFTEQ { RightShift }
  | LEFTROTATEEQ { LeftRotate }
  | RIGHTROTATEEQ { RightRotate }

arg:
  | e=expr { mkpos (ByValue e) }
  | REF x=lvalue { mkpos (ByRef x) }
  | hasmut=boption(REF) a=array_expr { mkpos (ByArray(a,mkpos (if hasmut then Mut else Const))) }

lexpr:
  | e=expr { mkpos (LExpression e) }

lvalue:
  | x=var_name { mkpos (Base x) }
  | l=lvalue e=brack(expr) { mkpos (ArrayEl(l, e)) }
  | l=lvalue DOT f=var_name { mkpos (StructEl(l, f)) }

expr:
  | e=paren(expr) { mkpos e.data }
  | b=BOOL { mkpos (if b then True else False) }
  | n=INT { mkpos (IntLiteral n) }
  | s=STRING {mkpos (StringLiteral s) }
  | lval=lvalue { mkpos (Lvalue lval) }
  | LEN lval=lvalue { mkpos (ArrayElLen lval) }
  | b=paren(base_type) e=expr { mkpos (IntCast(b, e)) }
  | op=unop e=expr %prec UNARYOP { mkpos (UnOp(op, e)) }
  | e1=expr op=binop e2=expr { mkpos (BinOp(op, e1, e2)) }
  | e1=expr QUESTION e2=expr COLON e3=expr { mkpos (TernOp(e1, e2, e3)) }
  | fn=fun_name args=plist(arg) { mkpos (FnCall(fn, args)) }
  | DECLASSIFY e=paren(expr) { mkpos (Declassify e) }

array_expr:
  | es=alist(expr) { mkpos (ArrayLit es) }
  | a=lvalue { mkpos (ArrayVar a) }
  | ARRZEROS l=paren(lexpr) { mkpos (ArrayZeros l) }
  | ARRCOPY a=paren(lvalue) { mkpos (ArrayCopy a) }
  | ARRVIEW LPAREN a=lvalue COMMA i=expr COMMA l=lexpr RPAREN { mkpos (ArrayView(a, i, l)) }
  | NOINIT l=paren(lexpr) { mkpos (ArrayNoinit l) }

base_variable_type:
  | b=base_type
    { mkpos (RefVT(b, mkpos Unknown, mkpos Const)) }
  | l=label b=base_type
    { mkpos (RefVT(b, l, mkpos Const)) }
  | m=mutability b=base_type
    { mkpos (RefVT(b, mkpos Unknown, m)) }
  | l=label m=mutability b=base_type
    { mkpos (RefVT(b, l, m)) }

array_variable_type:
  | a=array_type
    { mkpos (ArrayVT(a, mkpos Unknown, mkpos Const)) }
  | l=label a=array_type
    { mkpos (ArrayVT(a, l, mkpos Const)) }
  | m=mutability a=array_type
    { mkpos (ArrayVT(a, mkpos Unknown, m)) }
  | l=label m=mutability a=array_type
    { mkpos (ArrayVT(a, l, m)) }

%inline if_clause:
  | IF c=paren(expr) thens=block elses=loption(ELSE elses=else_clause { elses })
    { mkpos (If(c, thens, elses)) }

else_clause:
  | iff=if_clause
    { [iff] }
  | elses=block
    { elses }

statement:
  | b=base_variable_type x=var_name ASSIGN e=expr SEMICOLON
    { mkpos (BaseDec(x, b, e)) }
  | a=array_variable_type x=var_name ASSIGN ae=array_expr SEMICOLON
    { mkpos (ArrayDec(x, a, ae)) }
  | MUT STRUCT s=struct_name x=var_name SEMICOLON (* XXX make this better *)
    { mkpos (StructDec(x, s)) }
  | a=array_variable_type x=var_name ASSIGN n=var_name RIGHTARROW e=expr SEMICOLON
    { let { data=ArrayVT({ data=ArrayAT(b, l) }, _, _) } = a in
      mkpos (ArrayDec(x, a, mkposrange(n,e) (ArrayComp(b, l, n, e)))) }
  | lval=lvalue ASSIGN e=expr SEMICOLON
    { mkpos (Assign(lval, e)) }
  | lval=lvalue op=binopeq e=expr SEMICOLON
    { mkpos (Assign(lval, mkpos (BinOp(op, mkposof(lval) (Lvalue lval), e)))) }
  | iff=if_clause (* takes care of else ifs and elses too! *)
    { iff }
  | a=FOR LPAREN b=base_type i=var_name ASSIGN e1=expr TO e2=expr z=RPAREN stms=block
    { mkposrange(a,z) (For(i, b, e1, e2, stms)) }
  | fn=fun_name args=plist(arg) SEMICOLON
    { mkpos (VoidFnCall(fn, args)) }
  | RETURN e=expr SEMICOLON
    { mkpos (Return e) }
  | RETURN SEMICOLON
    { mkpos VoidReturn }

%inline block: xs=blist(statement) { xs }

ret_type:
  | VOID { None }
  | l=label b=base_type { Some (mkpos (BaseET(b, l))) }

param_type:
  | l=label b=base_type
    { mkpos (RefVT(b, l, mkpos Const)) }
  | l=label m=mutability b=base_type
    { mkpos (RefVT(b, l, m)) }
  | l=label a=array_type
    { mkpos (ArrayVT(a, l, mkpos Const)) }
  | l=label m=mutability a=array_type
    { mkpos (ArrayVT(a, l, m)) }
  | STRUCT s=struct_name
    { mkpos (StructVT(s, mkpos Const)) }
  | m=mutability STRUCT s=struct_name
    { mkpos (StructVT(s, m)) }

param:
  | t=param_type x=var_name
    { mkpos (Param(x, t)) }

field_type:
  | l=label b=base_type
    { mkpos (RefVT(b, l, mkpos Mut)) }
  | EMBED l=label b=base_type
    { mkpos (RefVT(b, l, mkpos Const)) }
  | l=label a=array_type
    { mkpos (ArrayVT(a, l, mkpos Mut)) }
  | EMBED l=label a=array_type
    { mkpos (ArrayVT(a, l, mkpos Const)) }
  | STRUCT s=struct_name
    { mkpos (StructVT(s, mkpos Mut)) }
  | EMBED STRUCT s=struct_name
    { mkpos (StructVT(s, mkpos Const)) }

field:
  | t=field_type x=var_name SEMICOLON
    { mkpos (Field(x, t)) }

function_dec:
  | export=boption(EXPORT) r=ret_type fn=fun_name params=plist(param) body=block
    { mkpos (FunDec(fn, {export; inline=Default}, r, params, body)) }
  | INLINE r=ret_type fn=fun_name params=plist(param) body=block
    { mkpos (FunDec(fn, {export=false; inline=Always}, r, params, body)) }
  | NOINLINE r=ret_type fn=fun_name params=plist(param) body=block
    { mkpos (FunDec(fn, {export=false; inline=Never}, r, params, body)) }
  | EXTERN r=ret_type fn=fun_name params=plist(param) SEMICOLON
    { mkpos (CExtern(fn, r, params)) }

struct_dec:
  | STRUCT s=struct_name fields=blist(field)
    { mkpos (Struct(s, fields)) }

fact_module:
  | fdec=function_dec
    { Module ([fdec], []) }
  | sdec=struct_dec
    { Module ([], [sdec]) }
  | fdec=function_dec m=fact_module
    { let Module(fdecs,sdecs) = m in
        Module (fdec :: fdecs, sdecs) }
  | sdec=struct_dec m=fact_module
    { let Module(fdecs,sdecs) = m in
        Module (fdecs, sdec :: sdecs) }

top_level:
  | FD_START fd=function_dec       { FunctionDec fd }
  | ST_START st=statement          { Statement st }
  | EX_START ex=expr EX_END        { Expression ex }
