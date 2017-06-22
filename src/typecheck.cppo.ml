open Pos
open Err

let wrap f pa = { pa with data=f pa.pos pa.data }

#define pfunction wrap @@ fun p -> function
#define mkpos make_ast p @@


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


(* Subtyping *)

let (<:) { data=b1 } { data=b2 } =
  match b1,b2 with
    | Tast.UInt n, Tast.UInt m when n = m -> true
    | Tast.Int n, Tast.Int m when n = m -> true
    | Tast.Bool, Tast.Bool -> true
    | Tast.Num n, Tast.Int k -> true
    | Tast.Num n, Tast.UInt k when n >= 0 -> true
    | _ -> false


let basetype = pfunction
  | Ast.RefVT(b, { data=Ast.Unknown; pos=p }, m) ->
    Tast.RefVT(bconv b, mkpos ref Tast.Public, mconv m)

let tc_expr = pfunction
  | Ast.True ->
    (Tast.True, Tast.BaseET(mkpos Tast.Bool, mkpos ref Tast.Public))
  | Ast.False ->
    (Tast.False, Tast.BaseET(mkpos Tast.Bool, mkpos ref Tast.Public))
  | Ast.IntLiteral n ->
    (Tast.IntLiteral n, Tast.BaseET(mkpos Tast.Num n, mkpos ref Tast.Public))

let tc_stm = pfunction
  | Ast.BaseDec(x,b,e) ->
    let {data=(ex,Tast.BaseET(ty,_))} as e' = tc_expr e in
    let {data=Tast.RefVT({data=Tast.Ref(xty)},_,_)} = basetype b in
      if not (ty <: xty) then raise @@ err (e'.pos);
      Tast.BaseDec(x,basetype b,e')

let tc_fdec = pfunction
  | Ast.FunDec(fn,rt,params,stms) ->
    Tast.FunDec(fn,rt,params,List.map tc_stm stms)

let tc_module (Ast.Module fdecs) =
  (Tast.Module (List.map tc_fdec fdecs))
