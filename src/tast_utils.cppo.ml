open Pos
open Err
open Tast

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

#define mkpos make_ast p @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function


(* Convenience *)

let make_blit p n =
  ((if n then True else False), BaseET(mkpos Bool, mkpos Fixed Public))

let make_nlit p n =
  (IntLiteral n, BaseET(mkpos Num(abs n, n < 0), mkpos Fixed Public))


(* Simple Predicates *)

let is_int = xfunction
  | UInt _ -> true
  | Int _ -> true
  | Num _ -> true
  | Bool -> false

let is_signed = xfunction
  | Int _ -> true
  | UInt _ -> false
  | Num(_,s) -> s
  | Bool -> false

let is_bool = xfunction
  | Bool -> true
  | _ -> false

let is_array = xfunction
  | BaseET _ -> false
  | ArrayET _ -> true


(* Extraction *)

let type_of' : (expr -> expr_type') = xfunction
  | (_,ty) -> ty
let type_of = rebind type_of'

let atype_of : (array_expr -> expr_type) = xfunction
  | (_,ty) -> mkpos ty

let atype_to_btype = xfunction
  | ArrayAT(b,_) -> b

let type_out = xfunction
  | BaseET(b,ml) -> (b,ml)
  | ArrayET(a,ml,_) -> (atype_to_btype a,ml)

let expr_to_btype : (expr -> base_type) = xfunction
  | (_,BaseET(b,_)) -> b

let expr_to_ml : (expr -> maybe_label) = xfunction
  | (_,BaseET(_,ml)) -> ml

let expr_to_types : (expr -> base_type * maybe_label) = xfunction
  | (_,BaseET(b,ml)) -> b,ml

let atype_out = xfunction
  | ArrayET(a,ml,_) -> a,ml

let aetype_to_lexpr' = xfunction
  | ArrayET(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      lexpr.data

let refvt_to_betype' = xfunction
  | ArrayVT(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      BaseET(bt,ml)
let refvt_to_betype = rebind refvt_to_betype'

let arrayvt_to_refvt = pfunction
  | ArrayVT(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      RefVT(bt,ml,m)

let refvt_type_out = xfunction
  | RefVT(b,ml,m) -> b,ml,m
  | ArrayVT(a,ml,m) -> (atype_to_btype a),ml,m

let refvt_to_lexpr = xfunction
  | ArrayVT(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      lexpr

let refvt_to_lexpr_option = xfunction
  | RefVT _ -> None
  | ArrayVT(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      Some lexpr.data
  | StructVT _ -> None

let refvt_to_etype' = xfunction
  | RefVT(b,ml,_) -> BaseET(b, ml)
  | ArrayVT(a,ml,m) -> ArrayET(a, ml, m)
let refvt_to_etype = rebind refvt_to_etype'

let fname_of = xfunction
  | FunDec(fname,_,_,_,_)
  | CExtern(fname,_,_)
  | DebugFunDec(fname,_,_) -> fname

let sname_of = xfunction
  | Struct(sname,_) -> sname

(* Subtyping *)

let (<:) { data=b1 } { data=b2 } =
  match b1,b2 with
    | UInt n, UInt m when n <= m -> true
    | Int n, Int m when n <= m -> true
    | Bool, Bool -> true
    | Num(k,s), Int n -> true
    | Int n, Num(k,s) -> true
    | Num(k,s), UInt n when not s -> true
    | UInt n, Num(k,s) when not s -> true
    | Num _, Num _ -> true
    | String, String -> true
    | _ -> false

let (=:) { data=b1 } { data=b2 } =
  match b1,b2 with
    | Num _, UInt _
    | Num _, Int _
    | UInt _, Num _
    | Int _, Num _ -> true
    | x, y when x = y -> true
    | _ -> false

let (<::) { data=ArrayAT(b1,lx1) } { data=ArrayAT(b2,lx2) } =
  let lxmatch =
    match lx1.data,lx2.data with
      | LIntLiteral n, LIntLiteral m when n = m -> true
      | LDynamic x, LDynamic y when x.data = y.data -> true
      | _ -> false in
    lxmatch && (b1 =: b2)

let joinable_bt b1 b2 =
  (b1 <: b2) or (b2 <: b1)

let join_bt p { data=b1 } { data=b2 } =
  let b' =
    match b1,b2 with
      | UInt n, UInt m -> UInt (max n m)
      | Int n, Int m -> Int (max n m)
      | Bool, Bool -> b1
      | Num(k,s), Int n -> b2
      | Int n, Num(k,s) -> b1
      | Num(k,s), UInt n -> b2
      | UInt n, Num(k,s) -> b1
      | String, String -> b1
      | Num(k1,s1), Num(k2,s2) -> Num(max k1 k2,s1 || s2) (* XXX max k1 k2 makes no sense *)
      | _ -> raise @@ cerr("type mismatch: " ^ show_base_type' b1 ^ " <> " ^ show_base_type' b2, p);
  in mkpos b'

let min_bt p { data=b1 } { data=b2 } =
  let b' =
    match b1,b2 with
      | UInt n, UInt m -> UInt (min n m)
      | Int n, Int m -> Int (min n m)
      | Num(k,s), Int n -> b2
      | Int n, Num(k,s) -> b1
      | Num(k,s), UInt n -> b2
      | UInt n, Num(k,s) -> b1
      | Num(k1,s1), Num(k2,s2) -> Num(max k1 k2,s1 || s2) (* XXX max k1 k2 makes no sense *)
      | _ -> raise @@ cerr("invalid types for min_bt: " ^ show_base_type' b1 ^ " <> " ^ show_base_type' b2, p);
  in mkpos b'

let meet_bt p { data=b1 } { data=b2 } =
  let b' =
    match b1,b2 with
      | UInt n, UInt m when n = m -> b1
      | Int n, Int m when n = m -> b1
      | Bool, Bool -> b1
      | Num(k,s), Int n -> b2
      | Int n, Num(k,s) -> b1
      | Num(k,s), UInt n when k >= 0 -> b2
      | UInt n, Num(k,s) when k >= 0 -> b1
      | String, String -> b1
      | Num(k1,s1), Num(k2,s2) -> Num(max k1 k2,s1 || s2)
      | _ -> raise @@ err(p)
  in mkpos b'

let (<$.) l1 l2 =
  match l1,l2 with
    | x, y when x = y -> true
    | Public, Secret -> true
    | _ -> false

let (+$.) l1 l2 =
  match l1,l2 with
    | Public, Public -> Public
    | Public, Secret -> Secret
    | Secret, Public -> Secret
    | Secret, Secret -> Secret

let (<$) { data=ml1 } { data=ml2 } =
  match ml1,ml2 with
    | Fixed x, Fixed y -> x <$. y
    | _ -> false

let join_ml p { data=ml1 } { data=ml2 } =
  let ml' =
    match ml1,ml2 with
      | Fixed x, Fixed y -> Fixed (x +$. y)
      | _ -> raise @@ err(p)
  in mkpos ml'

let (<:$) ty1 ty2 =
  match (is_array ty1),(is_array ty2) with
    | false, false ->
      let b1,ml1 = type_out ty1 in
      let b2,ml2 = type_out ty2 in
        (b1 <: b2) && (ml1 <$ ml2)
    | _ -> false

let join_ty' p ty1 ty2 =
  let b1,ml1 = type_out ty1 in
  let b2,ml2 = type_out ty2 in
  let b' = join_bt p b1 b2 in
  let ml' = join_ml p ml1 ml2 in
    BaseET(b', ml')

let (<*) m1 m2 =
  match m1,m2 with
    | Const, Mut -> false (* can't alias a const as a mut *)
    | _ -> true

let can_be_passed_to { pos=p; data=argty} {data=paramty} =
  match argty, paramty with
    | RefVT(_,_,m1), RefVT(_,_,m2) when m1.data <> m2.data -> false
    | RefVT(b1,l1,_), RefVT(b2,l2,_) ->
      (b1 <: b2) && (l1 <$ l2)
    | ArrayVT(a1,l1,m1), ArrayVT(a2,l2,m2) ->
      let ArrayAT(b1,lx1), ArrayAT(b2,lx2) = a1.data, a2.data in
      let lxmatch =
        match lx1.data, lx2.data with
          | _, LDynamic _ -> true
          | LIntLiteral n, LIntLiteral m when n = m -> true
          | _ -> false
      in
        (b1.data = b2.data) &&
        lxmatch &&
        (match m1.data, m2.data with
          | Const, Const -> l1 <$ l2
          | Mut, Mut -> l1.data = l2.data
          | _ -> false
        )

let (<:$*) (ty1,is_new_mem) ty2 =
  let ArrayET(a1,l1,m1) = ty1.data in
  let ArrayET(a2,l2,m2) = ty2.data in
  let ArrayAT(b1,lx1), ArrayAT(b2,lx2) = a1.data, a2.data in
  let lxmatch =
    match lx1.data, lx2.data with
      | _, LDynamic _ -> true
      | LIntLiteral n, LIntLiteral m when n = m -> true
      | _ -> false
  in
    (b1 <: b2) &&
    lxmatch &&
    (match m1.data, m2.data with
      | Const, Const -> l1 <$ l2
      | Mut, Const -> l1 <$ l2
      | Const, Mut -> is_new_mem && l1 <$ l2
      | Mut, Mut -> l1.data = l2.data
    )


(* Complex Predicates *)

let is_expr_secret e =
  let { data=(_,BaseET(_,{data=Fixed l})) } = e in
    l = Secret

let param_is_ldynamic = xfunction
  | Param(_,{data=vty'}) ->
    begin
      match vty' with
        | ArrayVT({data=ArrayAT(_,{data=LDynamic _})},_,_) -> true
        | _ -> false
    end


(* Simple Manipulation *)

let atype_update_lexpr lexpr' = pfunction
  | ArrayAT(bt,_) -> ArrayAT(bt, mkpos lexpr')

let aetype_update_lexpr' lexpr' = xfunction
  | ArrayET(a,ml,m) ->
    ArrayET(atype_update_lexpr lexpr' a, ml, m)

let aetype_update_mut' mut = function
  | ArrayET(a,ml,_) -> ArrayET(a, ml, mut)

let refvt_update_mut' mut = xfunction
  | RefVT(b,ml,_) -> RefVT(b, ml, mut)


(* Structs *)

let has_struct sdecs s =
  List.exists (fun {data=Struct(sn,_)} -> s.data = sn.data) sdecs

let find_struct sdecs s =
  List.find (fun {data=Struct(sn,_)} -> s.data = sn.data) sdecs
