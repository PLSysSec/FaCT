open Ast

module SS = Set.Make(String)

exception ConstPassError of string

type constxt = Constext of int * SS.t

let rec rename_expr v x = function
  | VarExp y -> VarExp (if y = v then x else y)
  | Unop(op, e) -> Unop(op, rename_expr v x e)
  | BinOp(op, e1, e2) -> BinOp(op, rename_expr v x e1, rename_expr v x e2)
  | Primitive _ as e -> e
  | CallExp(fn, es) -> CallExp(fn, List.map (rename_expr v x) es)
and rename_stmt v x = function
  | VarDec(y, t, e) -> VarDec((if y = v then x else y), t, rename_expr v x e)
  | Assign(y, e) -> Assign((if y = v then x else y), rename_expr v x e)
  | If(e, tstms, fstms) -> If(rename_expr v x e, rename_stms v x tstms, rename_stms v x fstms)
  | While(e, stms) -> While(rename_expr v x e, rename_stms v x stms)
  | Return e -> Return (rename_expr v x e)
and rename_stms v x = List.map (rename_stmt v x)

let rec rb_ite ctx e tstms fstms =
  let Constext(ctr, _) = ctx in
  let (ctr, b) = (ctr + 1, "b"^(string_of_int ctr)) in
  let ctx = Constext(ctr, SS.empty) in
  let (ctx, tstms) = rb_stms ctx tstms in
  let (ctx, fstms) = rb_stms ctx fstms in
  let rb_thing v (ctx, tstms, fstms, finals) =
    let Constext(ctr, assigns) = ctx in
    let (ctr, xt) = (ctr + 1, v^(string_of_int ctr)) in
    let (ctr, xf) = (ctr + 1, v^(string_of_int ctr)) in
    let tstms = VarDec(xt, Int, VarExp v)::(rename_stms v xt tstms) in
    let fstms = VarDec(xf, Int, VarExp v)::(rename_stms v xf fstms) in
    let finals = finals@[Assign(v, BinOp(B_Or, BinOp(B_And, VarExp(b), VarExp(xt)),
                                               BinOp(B_And, Unop(B_Not, VarExp(b)), VarExp(xf))))] in
    (Constext(ctr, assigns), tstms, fstms, finals)
  in
  let Constext(_, assigns) = ctx in
  let (ctx, tstms, fstms, finals) =
    SS.fold rb_thing assigns (Constext(ctr, assigns), tstms, fstms, []) in
  (ctx, VarDec(b, Bool, e)::(tstms@fstms@finals))

and rb_stmt ctx = function
  | If(e,tstms,fstms) -> rb_ite ctx e tstms fstms
  | Assign(v, e) as s ->
      let Constext(ctr, assigns) = ctx in
      (Constext(ctr, SS.add v assigns), [s])
  | _ as s -> (ctx, [s])

and rb_stms ctx = function
  | [] -> (ctx, [])
  | stmt::stms ->
      let (new_ctx, new_stms) = rb_stmt ctx stmt in
      let (new_ctx, new_stmss) = rb_stms new_ctx stms in
      (new_ctx, new_stms@new_stmss)

let remove_branch = function
  | FunctionDec(_,_,_,stms) ->
      rb_stms (Constext(1, SS.empty)) stms
