open Ast
open Llvm

let context = global_context ()
let the_module = create_module context "my cool compiler"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let env:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type' = double_type context
let string_type = const_stringz context

exception Error of string

let pass_prim = function
  | Number _ as n -> n

let pass_dec = function
  | FunctionDec _ as f -> f
  | VarDec _ as v -> v

(* Checks if the variant is in both branches *)
(* If so, collapses the if-statement *)
let remove_branch = function
  | Seq(Dec(VarDec (y, Primitive(Number n))),
        Seq(If(BinOp(GT,x,n'),
           Mutate(Variable y', BinOp(Plus,Variable y'',n'')),
           Mutate(Variable y''', BinOp(Minus,Variable y'''',n'''))),
        Return r)) as s
    (*when (y==y')&&(y==y'')&&(y==y''')&&(y==y'''')&&
         (n==n')&&(n==n'')&&(n==n''')*) ->
    Seq (
     Dec(VarDec("x_g_5", CallExp("BOOLIFY", [BinOp(GT,x,n')]))),
     Seq(Dec(VarDec("y", (BinOp (Plus,
                         Primitive(Number 3.0),
                         BinOp(Plus,
                               BinOp(B_And,n'',Variable("x_g_5")),
                               BinOp(B_And,n''', UnaryOp(B_Not,Variable("x_g_5")))))))),
     Return r))
  | _ -> raise (Error "We dun fucked up")

let rec pass_expr = function
  | Seq(Dec _,Seq(If _, Return _)) as s -> remove_branch s
  | Seq(e,e') -> Seq(e,e')
  | Primitive p -> Primitive(pass_prim p)
  | Variable _ as v -> v
  | BinOp _ as b -> b
  | UnaryOp _ as u -> u
  | If _ as i -> i
  | Mutate _ as m -> m
  | Dec d -> Dec(pass_dec d)
  | CallExp _ as c -> c
