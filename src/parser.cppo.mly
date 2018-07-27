%{
open Lexing
open Pos
open Err
open Ast

let to_type { data=t; pos=p } =
  { pos=p; data=
    match t with
      | "bool" -> BaseBool
      | "int8" -> BaseInt 8
      | "int16" -> BaseInt 16
      | "int32" -> BaseInt 32
      | "int64" -> BaseInt 64
      | "uint8" -> BaseUInt 8
      | "uint16" -> BaseUInt 16
      | "uint32" -> BaseUInt 32
      | "uint64" -> BaseUInt 64
      | "uint128" -> BaseUInt 128
      | _ as t -> raise (errParseType p t) }

let to_base_type l ({ pos=p } as t) =
  { pos=p; data=
    match (to_type t).data with
      | BaseBool -> Bool l
      | BaseUInt s -> UInt(s, l)
      | BaseInt s -> Int(s, l) }

let lit_to_type { data=t; pos=p } =
  { pos=p; data=
    match t with
      | "i8" -> BaseInt 8
      | "i16" -> BaseInt 16
      | "i32" -> BaseInt 32
      | "i64" -> BaseInt 64
      | "u8" -> BaseUInt 8
      | "u16" -> BaseUInt 16
      | "u32" -> BaseUInt 32
      | "u64" -> BaseUInt 64
      | "u128" -> BaseUInt 128
      | _ as t -> raise (errParseType p t) }
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
%token <string> LIT
%token FOR FROM TO IN
%token LBRACK RBRACK
%token LBRACE RBRACE

%token SECRET PUBLIC
%token CONST MUT
%token REF
%token RETURN
%token DECLASSIFY ASSUME
%token ARRZEROS ARRCOPY ARRVIEW NOINIT
%token SEMICOLON
%token COMMA
%token LEN RIGHTARROW
%token EXTERN INLINE EXPORT NOINLINE
%token STRUCT DOT
%token CACHELINE

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

label:
  | PUBLIC { mkpos Public }
  | SECRET { mkpos Secret }

basic_type:
  | t=TYPE { to_type (mkpos t) }

base_type:
  | l=label t=TYPE { to_base_type l (mkpos t) }
  | l=label MUT t=TYPE { mkpos (Ref(to_base_type l (mkpos t), mkpos RW)) }

  | l=label t=TYPE len=brack(lexpr) { mkpos (Arr(to_base_type l (mkpos t), len, default_var_attr)) }
  | l=label MUT t=TYPE len=brack(lexpr) { mkpos (Arr(mkpos (Ref(to_base_type l (mkpos t), mkpos RW)), len, default_var_attr)) }

  | l=label t=TYPE LBRACK RBRACK { mkpos (Arr(to_base_type l (mkpos t), mkpos LUnspecified, default_var_attr)) }
  | l=label MUT t=TYPE LBRACK RBRACK { mkpos (Arr(mkpos (Ref(to_base_type l (mkpos t), mkpos RW)), mkpos LUnspecified, default_var_attr)) }

  | CACHELINE b=base_type {
      match b.data with
        | Arr(base, len, vattr) ->
            { b with data=(Arr(base, len, { vattr with cache_aligned=true })) }
        | _ -> raise (errSyntax (to_pos $symbolstartpos $endpos)) }

  | l=label MUT t=TYPE LESSTHAN n=INT GREATERTHAN
    { let bw =
        match t with
            | "uint8" -> 8
            | "uint16" -> 16
            | "uint32" -> 32
            | "uint64" -> 64
      in
        mkpos (UVec(bw, n, l)) }

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

lexpr:
  | n=INT { mkpos (LIntLiteral n) }
  | e=expr { mkpos (LExpression e) }

fieldassign:
  | x=var_name COLON e=expr { (x, e) }

expr:
  | e=paren(expr) { mkpos e.data }
  | b=BOOL { mkpos (if b then True else False) }
  | n=INT { mkpos (UntypedIntLiteral n) }
  | n=INT t=LIT { mkpos (IntLiteral(n, lit_to_type(mkpos t))) }
  | x=var_name { mkpos (Variable x) }
  | LEN e=expr { mkpos (ArrayLen e) }
  | t=basic_type e=paren(expr) { mkpos (Cast(t, e)) }
  | op=unop e=expr %prec UNARYOP { mkpos (UnOp(op, e)) }
  | e1=expr op=binop e2=expr { mkpos (BinOp(op, e1, e2)) }
  | e1=expr QUESTION e2=expr COLON e3=expr { mkpos (TernOp(e1, e2, e3)) }
  | e1=expr QUESTION QUESTION e2=expr COLON COLON e3=expr { mkpos (Select(e1, e2, e3)) }
  | DECLASSIFY e=paren(expr) { mkpos (Declassify e) }
  | ASSUME e=paren(expr) { mkpos (Assume e) }

  | REF x=var_name { mkpos (Enref x) }
  | TIMES e=expr %prec UNARYOP { mkpos (Deref e) }
  | e=expr i=brack(lexpr) { mkpos (ArrayGet(e, i)) }
  | es=alist(expr) { mkpos (ArrayLit es) }
  | ARRZEROS l=paren(lexpr) { mkpos (ArrayZeros l) }
  | ARRCOPY e=paren(expr) { mkpos (ArrayCopy e) }
  | ARRVIEW LPAREN e=expr COMMA i=lexpr COMMA l=lexpr RPAREN { mkpos (ArrayView(e, i, l)) }

  | e=expr COLON LESSTHAN mask=separated_list(COMMA, INT) GREATERTHAN
    { mkpos (Shuffle(e, mask)) }

  | fs=alist(fieldassign) { mkpos (StructLit fs) }
  | e=expr DOT x=var_name { mkpos (StructGet(e, x)) }

  | s=STRING {mkpos (StringLiteral s) }

%inline if_clause:
  | IF c=paren(expr) thens=block elses=loption(ELSE elses=else_clause { elses })
    { mkpos (If(c, thens, elses)) }

else_clause:
  | iff=if_clause
    { [iff] }
  | elses=block
    { elses }

statement:
  | stms=block { mkpos (Block stms) }
  | b=base_type x=var_name ASSIGN e=expr SEMICOLON
    { mkpos (VarDec(x, b, e)) }
  | b=base_type x=var_name ASSIGN fn=fun_name args=plist(expr) SEMICOLON
    { mkpos (FnCall(x, b, fn, args)) }
  | fn=fun_name args=plist(expr) SEMICOLON
    { mkpos (VoidFnCall(fn, args)) }
  | e1=expr ASSIGN e2=expr SEMICOLON
    { mkpos (Assign(e1, e2)) }
  | e1=expr op=binopeq e2=expr SEMICOLON
    { let deref = mkposof(e1) (Deref e1) in
      let binop = mkpos (BinOp(op, deref, e2)) in
        mkpos (Assign(e1, binop)) }
  | iff=if_clause (* takes care of else ifs and elses too! *)
    { iff }
  | a=FOR LPAREN t=basic_type x=var_name FROM e1=expr TO e2=expr z=RPAREN stms=block
    { mkposrange(a,z) (RangeFor(x, t, e1, e2, stms)) }
  | a=FOR LPAREN t=basic_type x=var_name IN e=expr z=RPAREN stms=block
    { mkposrange(a,z) (ArrayFor(x, t, e, stms)) }
  | RETURN e=expr SEMICOLON
    { mkpos (Return e) }
  | RETURN SEMICOLON
    { mkpos VoidReturn }

%inline block: xs=blist(statement) { xs }

ret_type:
  | VOID { None }
  | b=base_type { Some b }

param:
  | t=base_type x=var_name
    { mkpos (Param(x, t)) }

function_dec:
  | export=boption(EXPORT) r=ret_type fn=fun_name params=plist(param) body=block
    { mkpos (FunDec(fn, {export; inline=Default}, r, params, body)) }
  | INLINE r=ret_type fn=fun_name params=plist(param) body=block
    { mkpos (FunDec(fn, {export=false; inline=Always}, r, params, body)) }
  | NOINLINE r=ret_type fn=fun_name params=plist(param) body=block
    { mkpos (FunDec(fn, {export=false; inline=Never}, r, params, body)) }
  | EXTERN r=ret_type fn=fun_name params=plist(param) SEMICOLON
    { mkpos (CExtern(fn, r, params)) }

field:
  | b=base_type x=var_name SEMICOLON
    { mkpos (Field(x, b)) }

struct_dec:
  | STRUCT s=struct_name fields=blist(field)
    { mkpos (StructDef(s, fields)) }

fact_module:
  | fdec=function_dec
    { Module ([], [fdec]) }
  | sdec=struct_dec
    { Module ([sdec], []) }
  | fdec=function_dec m=fact_module
    { let Module(sdecs,fdecs) = m in
        Module (sdecs, fdec :: fdecs) }
  | sdec=struct_dec m=fact_module
    { let Module(sdecs,fdecs) = m in
        Module (sdec :: sdecs, fdecs) }
