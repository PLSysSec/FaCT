open Pos
open Err
open Env

let wrap f pa = { pa with data=f pa.pos pa.data }
let xwrap f pa = f pa.pos pa.data

#define mkpos make_ast p @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function
(* g for 'uses Gamma' *)
#define gfunction fun venv -> pfunction


(* Trivial conversions *)

let xconv = pfunction
  | Ast.UInt n -> Tast.UInt n
  | Ast.Int n -> Tast.Int n
  | Ast.Bool -> Tast.Bool

let bconv = pfunction
  | Ast.Ref x -> Tast.Ref (xconv x)

let mconv = pfunction
  | Ast.Const -> Tast.Const
  | Ast.Mut -> Tast.Mut

let mlconv = pfunction
  | Ast.Public -> Tast.(Fixed Public)
  | Ast.Secret -> Tast.(Fixed Secret)
  | Ast.Unknown -> Tast.(Guess (ref Public))


(* Subtyping *)

let (<:) { data=b1 } { data=b2 } =
  match b1,b2 with
    | Tast.UInt n, Tast.UInt m when n = m -> true
    | Tast.Int n, Tast.Int m when n = m -> true
    | Tast.Bool, Tast.Bool -> true
    | Tast.Num k, Tast.Int n -> true
    | Tast.Num k, Tast.UInt n when k >= 0 -> true
    | _ -> false


(* Extraction *)

let expr_to_btype = xfunction
  | (_,Tast.BaseET(ty,_)) -> ty

let ref_to_btype = xfunction
  | Tast.Ref(xty) -> xty

let refvt_to_btype = xfunction
  | Tast.RefVT(xty,_,_) -> ref_to_btype xty

let refvt_to_bxtype = xfunction
  | Tast.RefVT(xty,ml,_) -> Tast.BaseET(ref_to_btype xty, ml)

let expr_to_ml = xfunction
  | (_,Tast.BaseET(_,ml)) -> ml


(* Actual typechecking *)

let basetype = pfunction
  | Ast.RefVT(b, l, m) ->
    Tast.RefVT(bconv b, mlconv l, mconv m)

let rec tc_expr = gfunction
  | Ast.True ->
    (Tast.True, Tast.(BaseET(mkpos Bool, mkpos Fixed Public)))
  | Ast.False ->
    (Tast.False, Tast.(BaseET(mkpos Bool, mkpos Fixed Public)))
  | Ast.IntLiteral n ->
    (Tast.IntLiteral n, Tast.(BaseET(mkpos Num n, mkpos Fixed Public)))
  | Ast.Variable x ->
    let b = find_var venv x in
      (Tast.Variable x, refvt_to_bxtype b)
  | Ast.IntCast(b,e) ->
    let b' = xconv b in
    let e' = tc_expr venv e in
    let ml = expr_to_ml e' in
      (Tast.IntCast(b',e'), Tast.(BaseET(b',ml)))

let tc_stm = gfunction
  | Ast.BaseDec(x,b,e) ->
    let e' = tc_expr venv e in
    let ty = expr_to_btype e' in
    let b' = basetype b in
    let xty = refvt_to_btype b' in
      if not (ty <: xty) then raise @@ err e';
      add_var venv x b';
      Tast.BaseDec(x,b',e')

let tc_fdec = pfunction
  | Ast.FunDec(fn,rt,params,stms) ->
    let venv = Env.new_env () in
      Tast.FunDec(fn,rt,params,List.map (tc_stm venv) stms)

let tc_module (Ast.Module fdecs) =
  (Tast.Module (List.map tc_fdec fdecs))
