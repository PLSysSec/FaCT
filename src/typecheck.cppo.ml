open Pos
open Err
open Env

#define err(p) InternalCompilerError("from source" ^ __LOC__ << p)

let wrap f pa = { pa with data=f pa.pos pa.data }
let xwrap f pa = f pa.pos pa.data

let rebind f pa = { pa with data=f pa }

#define mkpos make_ast p @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function


(* Predicates *)

let is_int = xfunction
  | Tast.UInt _ -> true
  | Tast.Int _ -> true
  | _ -> false


(* Trivial conversions *)

let bconv = pfunction
  | Ast.UInt n -> Tast.UInt n
  | Ast.Int n -> Tast.Int n
  | Ast.Bool -> Tast.Bool

let mlconv = pfunction
  | Ast.Public -> Tast.(Fixed Public)
  | Ast.Secret -> Tast.(Fixed Secret)
  | Ast.Unknown -> raise (LabelError("Label inference not yet implemented!" << p))

let mconv = pfunction
  | Ast.Const -> Tast.Const
  | Ast.Mut -> Tast.Mut

let rconv = pfunction
  | Ast.Ref x -> Tast.Ref (bconv x)

let refvt_conv = pfunction
  | Ast.RefVT(r,l,m) ->
    Tast.RefVT(rconv r, mlconv l, mconv m)


(* Extraction *)

let type_of = xfunction
  | (_,ty) -> mkpos ty

let type_out' = xfunction
  | Tast.BaseET(b,ml) -> (b,ml)

let expr_to_ml = xfunction
  | (_,Tast.BaseET(_,ml)) -> ml

let expr_to_btype = xfunction
  | (_,Tast.BaseET(b,_)) -> b

let ref_to_btype = xfunction
  | Tast.Ref(r) -> r

let refvt_to_btype = xfunction
  | Tast.RefVT(r,_,_) -> ref_to_btype r

let refvt_to_etype' = xfunction
  | Tast.RefVT(r,ml,_) -> Tast.BaseET(ref_to_btype r, ml)
let refvt_to_etype = rebind refvt_to_etype'


(* Subtyping *)

let (<:) { data=b1 } { data=b2 } =
  match b1,b2 with
    | Tast.UInt n, Tast.UInt m when n = m -> true
    | Tast.Int n, Tast.Int m when n = m -> true
    | Tast.Bool, Tast.Bool -> true
    | Tast.Num k, Tast.Int n -> true
    | Tast.Num k, Tast.UInt n when k >= 0 -> true
    | _ -> false

let (<$) { data=ml1 } { data=ml2 } =
  match ml1,ml2 with
    | Tast.Fixed x, Tast.Fixed y when x = y -> true
    | Tast.Fixed Public, Tast.Fixed Secret -> true
    | _ -> false

let (<:$) ty1 ty2 =
  let b1,ml1 = type_out' ty1 in
  let b2,ml2 = type_out' ty2 in
    (b1 <: b2) && (ml1 <$ ml2)


(* Actual typechecking *)

let rec tc_expr venv = pfunction
  | Ast.True ->
    (Tast.True, Tast.(BaseET(mkpos Bool, mkpos Fixed Public)))
  | Ast.False ->
    (Tast.False, Tast.(BaseET(mkpos Bool, mkpos Fixed Public)))
  | Ast.IntLiteral n ->
    (Tast.IntLiteral n, Tast.(BaseET(mkpos Num n, mkpos Fixed Public)))
  | Ast.Variable x ->
    let xref = find_var venv x in
      (Tast.Variable x, refvt_to_etype' xref)
  | Ast.IntCast(b,e) ->
    let b' = bconv b in
      if not (is_int b') then raise @@ err(b'.pos);
    let e' = tc_expr venv e in
      if not (is_int (expr_to_btype e')) then raise @@ err(e'.pos);
    let ml = expr_to_ml e' in
      (Tast.IntCast(b',e'), Tast.(BaseET(b',ml)))
  | Ast.Declassify e ->
    let e' = tc_expr venv e in
      (Tast.Declassify e', Tast.(BaseET(expr_to_btype e', mkpos Fixed Public)))

let tc_stm venv = pfunction
  | Ast.BaseDec(x,vt,e) ->
    let e' = tc_expr venv e in
    let ety = type_of e' in
    let vt' = refvt_conv vt in
    let xty = refvt_to_etype vt' in
      if not (ety <:$ xty) then raise @@ err(e'.pos);
      add_var venv x vt';
      Tast.BaseDec(x,vt',e')

let tc_fdec = pfunction
  | Ast.FunDec(fn,rt,params,stms) ->
    let venv = Env.new_env () in
      Tast.FunDec(fn,rt,params,List.map (tc_stm venv) stms)

let tc_module (Ast.Module fdecs) =
  (Tast.Module (List.map tc_fdec fdecs))
