open Pos
open Err
open Tast

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


(* Trivial conversions *)

let bconv = pfunction
  | Ast.UInt n -> UInt n
  | Ast.Int n -> Int n
  | Ast.Bool -> Bool

let lexprconv = pfunction
  | Ast.LIntLiteral n -> LIntLiteral n
  | Ast.LUnspecified -> raise @@ err(p)

let aconv = pfunction
  | Ast.ArrayAT(b,lexpr) -> ArrayAT(bconv b, lexprconv lexpr)

let mlconv = pfunction
  | Ast.Public -> Fixed Public
  | Ast.Secret -> Fixed Secret
  | Ast.Unknown -> raise (LabelError("Label inference not yet implemented!" << p))

let mconv = pfunction
  | Ast.Const -> Const
  | Ast.Mut -> Mut

let etype_conv = pfunction
  | Ast.BaseET(b,l) ->
    BaseET(bconv b, mlconv l)

let refvt_conv = pfunction
  | Ast.RefVT(b,l,m) ->
    RefVT(bconv b, mlconv l, mconv m)
  | Ast.ArrayVT(a,l,m) ->
    ArrayVT(aconv a, mlconv l, mconv m)

let atype_conv_fill lexpr' = pfunction
  | Ast.ArrayAT(bt,({data=LIntLiteral _} as le)) ->
    ArrayAT(bconv bt, lexprconv le)
  | Ast.ArrayAT(bt,{data=LUnspecified}) ->
    ArrayAT(bconv bt, mkpos lexpr')

let refvt_conv_fill lexpr' = pfunction
  | Ast.RefVT(b,l,m) ->
    RefVT(bconv b, mlconv l, mconv m)
  | Ast.ArrayVT(a,ml,m) ->
    ArrayVT(atype_conv_fill lexpr' a, mlconv ml, mconv m)


(* Extraction *)

let type_of = xfunction
  | (_,ty) -> mkpos ty

let atype_of = xfunction
  | (_,ty) -> mkpos ty

let type_out = xfunction
  | BaseET(b,ml) -> (b,ml)

let expr_to_btype = xfunction
  | (_,BaseET(b,_)) -> b

let expr_to_ml = xfunction
  | (_,BaseET(_,ml)) -> ml

let expr_to_types = xfunction
  | (_,BaseET(b,ml)) -> b,ml

let atype_to_lexpr' = xfunction
  | ArrayET(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      lexpr.data

let refvt_to_betype' = xfunction
  | ArrayVT(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      BaseET(bt,ml)

let refvt_to_lexpr = xfunction
  | ArrayVT(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      lexpr

let refvt_to_lexpr_option = xfunction
  | RefVT _ -> None
  | ArrayVT(a,ml,m) ->
    let ArrayAT(bt,lexpr) = a.data in
      Some lexpr.data

let refvt_to_etype' = xfunction
  | RefVT(b,ml,_) -> BaseET(b, ml)
  | ArrayVT(a,ml,m) -> ArrayET(a, ml, m)
let refvt_to_etype = rebind refvt_to_etype'

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


(* Subtyping *)

let (<:) { data=b1 } { data=b2 } =
  match b1,b2 with
    | UInt n, UInt m when n = m -> true
    | Int n, Int m when n = m -> true
    | Bool, Bool -> true
    | Num(k,s), Int n -> true
    | Int n, Num(k,s) -> true
    | Num(k,s), UInt n when not s -> true
    | UInt n, Num(k,s) when not s -> true
    | Num _, Num _ -> true
    | _ -> false

let join_bt p { data=b1 } { data=b2 } =
  let b' =
    match b1,b2 with
      | UInt n, UInt m when n = m -> b1
      | Int n, Int m when n = m -> b1
      | Bool, Bool -> b1
      | Num(k,s), Int n -> b2
      | Int n, Num(k,s) -> b1
      | Num(k,s), UInt n when not s -> b2
      | UInt n, Num(k,s) when not s -> b1
      | Num(k1,s1), Num(k2,s2) -> Num(max k1 k2,s1 || s2)
      | _ -> raise @@ err(p)
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
      | Num(k1,s1), Num(k2,s2) -> Num(max k1 k2,s1 || s2)
      | _ -> raise @@ err(p)
  in mkpos b'

let (<$) { data=ml1 } { data=ml2 } =
  match ml1,ml2 with
    | Fixed x, Fixed y when x = y -> true
    | Fixed Public, Fixed Secret -> true
    | _ -> false

let join_ml p { data=ml1 } { data=ml2 } =
  let ml' =
    match ml1,ml2 with
      | Fixed x, Fixed y when x = y -> ml1
      | Fixed Public, Fixed Secret
      | Fixed Secret, Fixed Public -> (Fixed Secret)
      | _ -> raise @@ err(p)
  in mkpos ml'

let (<:$) ty1 ty2 =
  let b1,ml1 = type_out ty1 in
  let b2,ml2 = type_out ty2 in
    (b1 <: b2) && (ml1 <$ ml2)

let join_ty' p ty1 ty2 =
  let b1,ml1 = type_out ty1 in
  let b2,ml2 = type_out ty2 in
  let b' = join_bt p b1 b2 in
  let ml' = join_ml p ml1 ml2 in
    BaseET(b', ml')


(* Actual typechecking *)

let tc_unop' p op e =
  let b,ml = expr_to_types e in
    begin
      match op with
        | Ast.Neg ->
          if not (is_int b) then raise @@ err(p);
        | Ast.BitwiseNot ->
          if not (is_int b) then raise @@ err(p);
        | Ast.LogicalNot ->
          if not (is_bool b) then raise @@ err(p);
    end;
    (UnOp(op, e), BaseET(b, ml))

let tc_binop_check p op b1 b2 =
  let yes _ = true in
  let pred1,pred2 =
    match op with
      | Ast.Equal
      | Ast.NEqual -> yes,yes
      | Ast.Plus
      | Ast.Minus
      | Ast.Multiply
      | Ast.GT
      | Ast.GTE
      | Ast.LT
      | Ast.LTE
      | Ast.BitwiseAnd
      | Ast.BitwiseOr
      | Ast.BitwiseXor
      | Ast.LeftShift
      | Ast.RightShift -> is_int,is_int
      | Ast.LogicalAnd
      | Ast.LogicalOr -> is_bool,is_bool
  in
    if not (pred1 b1) then raise @@ err(p);
    if not (pred2 b2) then raise @@ err(p);
    ()

let tc_binop' p op e1 e2 =
  let b1,ml1 = expr_to_types e1 in
  let b2,ml2 = expr_to_types e2 in
    tc_binop_check p op b1 b2;
  let b' =
    match op with
      | Ast.Plus
      | Ast.Minus
      | Ast.Multiply
      | Ast.GT
      | Ast.GTE
      | Ast.LT
      | Ast.LTE
      | Ast.BitwiseOr
      | Ast.BitwiseXor
      | Ast.BitwiseAnd
      | Ast.LogicalAnd
      | Ast.LogicalOr
      | Ast.Equal
      | Ast.NEqual ->
        join_bt p b1 b2
      | Ast.LeftShift
      | Ast.RightShift ->
        { b1 with pos=p }
  in
  let ml' = join_ml p ml1 ml2 in
    (BinOp(op, e1, e2), BaseET(b', ml'))

let rec tc_arg fenv venv = pfunction
  (* XXX should convert arrays to ByArray of ArrayVar *)
  | Ast.ByValue e ->
    ByValue (tc_expr fenv venv e)
  | Ast.ByRef x ->
    ByRef x
  | Ast.ByArray aexpr ->
    ByArray (tc_arrayexpr fenv venv aexpr)

and tc_args fenv venv p params args =
  match params,args with
    | [], [] -> []
    | (param::params), (arg::args) ->
      (* XXX check that types match *)
      let arg' = tc_arg fenv venv arg in
      if param_is_ldynamic param then
        let _::params = params in
        let lexpr' =
          begin
            match arg'.data with
              | ByValue {data=(Variable _,atype')} ->
                atype_to_lexpr' (mkpos atype')
              | ByRef x ->
                let refvt = Env.find_var venv x in
                  (refvt_to_lexpr refvt).data
              | ByArray {data=(_,atype')} ->
                atype_to_lexpr' (mkpos atype')
          end in
        let len =
          begin
            match lexpr' with
              | LIntLiteral n ->
                ByValue (mkpos (IntLiteral n, BaseET(mkpos Num(abs n,n < 0), mkpos Fixed Public)))
              | LDynamic lx ->
                ByValue (mkpos (Variable lx, BaseET(mkpos UInt 32, mkpos Fixed Public)))
          end in
          arg' :: (mkpos len) :: tc_args fenv venv p params args
      else
        arg' :: tc_args fenv venv p params args
    | _ -> raise @@ err(p)

and tc_expr fenv venv = pfunction
  | Ast.True ->
    (True, BaseET(mkpos Bool, mkpos Fixed Public))
  | Ast.False ->
    (False, BaseET(mkpos Bool, mkpos Fixed Public))
  | Ast.IntLiteral n ->
    (IntLiteral n, BaseET(mkpos Num(abs n,n < 0), mkpos Fixed Public))
  | Ast.Variable x ->
    let xref = Env.find_var venv x in
      (Variable x, refvt_to_etype' xref)
  | Ast.ArrayGet(x,e) ->
    let xref = Env.find_var venv x in
    let e' = tc_expr fenv venv e in
      (ArrayGet(x,e'), refvt_to_betype' xref)
  | Ast.ArrayLen x ->
    (* XXX type should be size_t not uint32 *)
    let xref = Env.find_var venv x in
    let lexpr = refvt_to_lexpr xref in
      begin
        match lexpr.data with
          | LIntLiteral n ->
            (IntLiteral n, BaseET(mkpos Num(abs n,n < 0), mkpos Fixed Public))
          | LDynamic len ->
            (Variable len, BaseET(mkpos UInt 32, mkpos Fixed Public))
      end
  | Ast.IntCast(b,e) ->
    let b' = bconv b in
      if not (is_int b') then raise @@ err(b'.pos);
    let e' = tc_expr fenv venv e in
      if not (is_int (expr_to_btype e')) then raise @@ err(e'.pos);
    let ml = expr_to_ml e' in
      (IntCast(b',e'), BaseET(b',ml))
  | Ast.Declassify e ->
    let e' = tc_expr fenv venv e in
      (Declassify e', BaseET(expr_to_btype e', mkpos Fixed Public))
  | Ast.UnOp(op,e) ->
    let e' = tc_expr fenv venv e in
      tc_unop' p op e'
  | Ast.BinOp(op,e1,e2) ->
    let e1' = tc_expr fenv venv e1 in
    let e2' = tc_expr fenv venv e2 in
      tc_binop' p op e1' e2'
  | Ast.TernOp(e1,e2,e3) ->
    let e1' = tc_expr fenv venv e1 in
      if not (is_bool (expr_to_btype e1')) then raise @@ err(e1'.pos);
    let e2' = tc_expr fenv venv e2 in
    let e3' = tc_expr fenv venv e3 in
      (TernOp(e1',e2',e3'), join_ty' p (type_of e2') (type_of e3'))
  | Ast.FnCall(f,args) ->
    let (FunDec(_,Some rty,params,_)) = (Env.find_var fenv f).data in
    let args' = tc_args fenv venv p params args in
      (FnCall(f,args'), rty.data)

and tc_arrayexpr fenv venv = pfunction
  | Ast.ArrayLit exprs ->
    (* XXX check that all expr types are compatible *)
    let exprs' = List.map (tc_expr fenv venv) exprs in
    let b = expr_to_btype @@ List.hd exprs' in (* XXX should be join of all exprs' *)
    let at' = mkpos ArrayAT(b, mkpos LIntLiteral(List.length exprs')) in
      (ArrayLit exprs', ArrayET(at', mkpos Fixed Public (* XXX should be join of all exprs' *), mkpos Const))
  | Ast.ArrayZeros lexpr ->
    (* XXX check that type is compatible *)
    let b = mkpos Num(0, false) in
    let lexpr' = lexprconv lexpr in
    let at' = mkpos ArrayAT(b, lexpr') in
    (ArrayZeros lexpr', ArrayET(at', mkpos Fixed Public, mkpos Const))
  | Ast.ArrayCopy x ->
    let ae' = refvt_to_etype' (Env.find_var venv x) in
      (ArrayCopy x, ae')
  | Ast.ArrayView(x,e,lexpr) ->
    let e' = tc_expr fenv venv e in
    let lexpr' = lexprconv lexpr in
    let ae = refvt_to_etype (Env.find_var venv x) in
    let ae' = aetype_update_lexpr' lexpr'.data ae in
      (ArrayView(x,e',lexpr'), ae')
  | Ast.ArrayComp(b,lexpr,x,e) ->
    let b' = bconv b in
    let lexpr' = lexprconv lexpr in
    let e' = tc_expr fenv venv e in
    let ae = ArrayET(mkpos ArrayAT(b', lexpr'), expr_to_ml e', mkpos Const) in
      (ArrayComp(b',lexpr',x,e'), ae)

let rec tc_stm fenv venv = pfunction

  | Ast.BaseDec(x,vt,e) ->
    let e' = tc_expr fenv venv e in
    let ety = type_of e' in
    let vt' = refvt_conv vt in
    let xty = refvt_to_etype vt' in
      if not (ety <:$ xty) then raise @@ err(e'.pos);
      Env.add_var venv x vt';
      BaseDec(x,vt',e')

  | Ast.BaseAssign(x,e) ->
    let e' = tc_expr fenv venv e in
      BaseAssign(x,e')

  | Ast.ArrayDec(x,vt,ae) ->
    let ae' = tc_arrayexpr fenv venv ae in
    let aty = atype_of ae' in
    (* if vt is LUnspecified then take it from aty *)
    let ae_lexpr' = atype_to_lexpr' aty in
    let vt' = refvt_conv_fill ae_lexpr' vt in
    let xty = refvt_to_etype vt' in
      (* XXX check that types match *)
      Env.add_var venv x vt';
      ArrayDec(x,vt',ae')

  | Ast.ArrayAssign(x,n,e) ->
    let n' = tc_expr fenv venv n in
    let e' = tc_expr fenv venv e in
      ArrayAssign(x,n',e')

  | Ast.If(cond,thenstms,elsestms) ->
    let cond' = tc_expr fenv venv cond in
    let thenstms' = tc_block fenv (Env.sub_env venv) thenstms in
    let elsestms' = tc_block fenv (Env.sub_env venv) elsestms in
      If(cond',thenstms',elsestms')

  | Ast.For(i,ity,lo,hi,stms) ->
    let ity' = bconv ity in
    let lo' = tc_expr fenv venv lo in
    let hi' = tc_expr fenv venv hi in
    let venv' = Env.sub_env venv in
      Env.add_var venv' i (mkpos RefVT(ity',mkpos Fixed Public,mkpos Const));
      let stms' = tc_block fenv venv' stms in
        For(i,ity',lo',hi',stms')

  | Ast.VoidFnCall(f,args) ->
    let (FunDec(_,Some rty,params,_)) = (Env.find_var fenv f).data in
    let args' = tc_args fenv venv p params args in
      VoidFnCall(f,args')

  | Ast.Return e ->
    let e' = tc_expr fenv venv e in
      Return e'

  | Ast.VoidReturn -> VoidReturn

and tc_block fenv venv stms =
  let stms' = List.map (tc_stm fenv venv) stms in
    (venv, stms')

let tc_param' = xfunction
  | Ast.Param(x,vty) ->
    let len = "__" ^ x.data ^ "_len" in
    let lexpr' = LDynamic(mkpos len) in
    (* the lexpr will only get used if vty is LUnspecified *)
    let refvt = refvt_conv_fill lexpr' vty in
    let param = Param(x,refvt) in
    let lexpr = refvt_to_lexpr_option refvt in
      param :: (match lexpr with
                 | None
                 | Some LIntLiteral _ -> []
                 | Some LDynamic len ->
                   let lenvt = mkpos RefVT(mkpos UInt 32, mkpos Fixed Public, mkpos Const) in
                     [Param(len, lenvt)])
let tc_param pa = List.map (make_ast pa.pos) (tc_param' pa)

let tc_fdec' fenv = function
  | Ast.FunDec(f,Some rt,params,stms) ->
    let rt' = etype_conv rt in
    let params' = List.flatten @@ List.map tc_param params in
    let venv = Env.new_env () in
      List.iter (fun {data=Param(name,vty)} ->
                  Env.add_var venv name vty)
        params';
      FunDec(f,Some rt',params',tc_block fenv venv stms)

let tc_fdec fenv = xfunction
  | Ast.FunDec(f,_,_,_) as fdec ->
    let fdec' = mkpos tc_fdec' fenv fdec in
      Env.add_var fenv f fdec';
      fdec'

let tc_module (Ast.Module fdecs) =
  let fenv = Env.new_env () in
  let ret = Module (fenv, List.map (tc_fdec fenv) fdecs) in
    (*print_endline (Env.show_env pp_function_dec fenv);*)
    ret
