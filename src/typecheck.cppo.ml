open Pos
open Err
open Tast

#define err(p) InternalCompilerError("from source " ^ __LOC__ << p)
#define cerr(msg, p) InternalCompilerError((msg) ^ " @ " ^ __FILE__ ^ ":" ^ string_of_int __LINE__ << p)

let wrap f pa = { pa with data=f pa.pos pa.data }
let xwrap f pa = f pa.pos pa.data

let rebind f pa = { pa with data=f pa }

#define mkpos make_ast p @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function

type tc_ctx_record = {
  rp   : label' ref;
  pc   : label';
  venv : (var_name * variable_type) Env.env;
  fenv : function_dec Env.env;
}

let make_fresh =
  let ctr = ref 0 in
  let make_fresh' name =
    ctr := !ctr + 1;
    "__v" ^ (string_of_int !ctr) ^ "_" ^ name
  in
    make_fresh'

let add_new_var venv x vt =
  let x' = { x with data=make_fresh x.data } in
  let entry = (x', vt) in
    Env.add_var venv x entry;
    Env.add_var venv x' entry;
    x'


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

let is_array = xfunction
  | BaseET _ -> false
  | ArrayET _ -> true

let param_is_ldynamic = xfunction
  | Param(_,{data=vty'}) ->
    begin
      match vty' with
        | ArrayVT({data=ArrayAT(_,{data=LDynamic _})},_,_) -> true
        | _ -> false
    end


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


(* Extraction *)

let type_of = xfunction
  | (_,ty) -> mkpos ty

let atype_of = xfunction
  | (_,ty) -> mkpos ty

let atype_to_btype = xfunction
  | ArrayAT(b,_) -> b

let type_out = xfunction
  | BaseET(b,ml) -> (b,ml)
  | ArrayET(a,ml,_) -> (atype_to_btype a,ml)

let expr_to_btype = xfunction
  | (_,BaseET(b,_)) -> b

let expr_to_ml = xfunction
  | (_,BaseET(_,ml)) -> ml

let expr_to_types = xfunction
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

let refvt_to_etype' = xfunction
  | RefVT(b,ml,_) -> BaseET(b, ml)
  | ArrayVT(a,ml,m) -> ArrayET(a, ml, m)
let refvt_to_etype = rebind refvt_to_etype'

let argtype_of venv = xfunction
  | ByValue e ->
    let b,ml = expr_to_types e in
      mkpos RefVT(b,ml,mkpos Const)
  | ByRef x ->
    let _,vt = Env.find_var venv x in
      vt
  | ByArray({data=(aexpr,aty)}, mut) ->
    let b,ml = atype_out (mkpos aty) in
    mkpos ArrayVT(b,ml,mut)


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

let (<::) { data=ArrayAT(b1,lx1) } { data=ArrayAT(b2,lx2) } =
  let lxmatch =
    match lx1.data,lx2.data with
      | LIntLiteral n, LIntLiteral m when n = m -> true
      | LDynamic x, LDynamic y when x.data = y.data -> true
      | _ -> false in
    lxmatch && (b1 = b2)

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
      | _ -> raise @@ cerr("type mismatch: " ^ show_base_type' b1 ^ " <> " ^ show_base_type' b2, p);
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
    | true, true ->
      let a1,ml1 = atype_out ty1 in
      let a2,ml2 = atype_out ty2 in
        (a1 <:: a2) && (ml1 <$ ml2)
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
    | ArrayVT(_,_,m1), ArrayVT(_,_,m2) when m1.data <> m2.data ->
      Log.warn "FIXME: Unsafe stuff here. Use at own risk";
      true
    | RefVT(b1,l1,_), RefVT(b2,l2,_) ->
      (b1 <: b2) && (l1 <$ l2)
    | ArrayVT(a1,l1,_), ArrayVT(a2,l2,_) ->
      let ArrayAT(b1,lx1), ArrayAT(b2,lx2) = a1.data, a2.data in
      let lxmatch =
        match lx1.data, lx2.data with
          | _, LDynamic _ -> true
          | LIntLiteral n, LIntLiteral m when n = m -> true
          | _ -> false
      in
        (b1.data = b2.data) && lxmatch && (l1 <$ l2)


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
  match op with
    | Ast.Equal
    | Ast.NEqual ->
      if (is_bool b1 && not (is_bool b2))
      || (is_bool b2 && not (is_bool b1))
      then raise @@ err(p)
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
    | Ast.RightShift ->
      if not (is_int b1) || not (is_int b2) then raise @@ err(p)
    | Ast.LogicalAnd
    | Ast.LogicalOr ->
      if not (is_bool b1) || not (is_bool b2) then raise @@ err(p)

let tc_binop' p op e1 e2 =
  let b1,ml1 = expr_to_types e1 in
  let b2,ml2 = expr_to_types e2 in
    tc_binop_check p op b1 b2;
  let b' =
    match op with
      | Ast.Plus
      | Ast.Minus
      | Ast.Multiply
      | Ast.BitwiseOr
      | Ast.BitwiseXor
      | Ast.BitwiseAnd
      | Ast.LogicalAnd
      | Ast.LogicalOr
      | Ast.Equal
      | Ast.NEqual ->
        join_bt p b1 b2
      | Ast.GT
      | Ast.GTE
      | Ast.LT
      | Ast.LTE ->
        mkpos Bool
      | Ast.LeftShift
      | Ast.RightShift ->
        { b1 with pos=p }
  in
  let ml' = join_ml p ml1 ml2 in
    (BinOp(op, e1, e2), BaseET(b', ml'))

let rec tc_arg tc_ctx = pfunction
  | Ast.ByValue e ->
    begin
      match e.data with
        | Ast.Variable x ->
          let _,vty = Env.find_var tc_ctx.venv x in
            begin
              match vty.data with
                | RefVT _ -> ByValue (tc_expr tc_ctx e)
                | ArrayVT _ ->
                  let ae' = tc_arrayexpr tc_ctx (mkpos Ast.ArrayVar x) in
                  let (_,ArrayET(_,_,mut)) = ae'.data in
                    if not (mut.data <* Const) then raise @@ err(p);
                    ByArray(ae', mkpos Const)
            end
        | _ -> ByValue (tc_expr tc_ctx e)
    end
  | Ast.ByRef x ->
    let x',xref = Env.find_var tc_ctx.venv x in
      begin
        match xref.data with
          | RefVT _ -> ByRef x'
          | ArrayVT _ ->
            let ae' = tc_arrayexpr tc_ctx (mkpos Ast.ArrayVar x) in
            let (_,ArrayET(_,_,mut)) = ae'.data in
              if not (mut.data <* Mut) then raise @@ cerr("variable `" ^ x.data ^ "` is not mut; ", p);
              ByArray(ae', mkpos Mut)
      end
  | Ast.ByArray(arr_expr, mutability) ->
    let m' = mconv mutability in
    let ae' = tc_arrayexpr tc_ctx arr_expr in
    let (_,ArrayET(_,_,mut)) = ae'.data in
    if not (mut.data <* m'.data) then raise @@ cerr("array expression is not proper mutability; ", p);
    ByArray(ae', m')

and tc_args xf_args tc_ctx p params args =
  match params,args with
    | [], [] -> []
    | (param::params), (arg::args) ->
      let arg' = tc_arg tc_ctx arg in
      let argref = argtype_of tc_ctx.venv arg' in
      let Param(_,paramvt) = param.data in
      Log.error "Expected: %s" (show_variable_type paramvt);
      Log.error "Actual: %s" (show_variable_type argref);
        if not @@ can_be_passed_to argref paramvt then raise @@ err(arg'.pos);
        if param_is_ldynamic param && xf_args then
          let _::params = params in
          let ByArray({data=(_,atype')},_) = arg'.data in
          let lexpr' = aetype_to_lexpr' (mkpos atype') in
          let len =
            match lexpr' with
              | LIntLiteral n ->
                ByValue (mkpos (IntLiteral n, BaseET(mkpos Num(abs n,n < 0), mkpos Fixed Public)))
              | LDynamic lx ->
                ByValue (mkpos (Variable lx, BaseET(mkpos UInt 32, mkpos Fixed Public)))
          in
            arg' :: (mkpos len) :: tc_args xf_args tc_ctx p params args
        else
          arg' :: tc_args xf_args tc_ctx p params args
    | _ -> raise @@ err(p)

and tc_expr tc_ctx = pfunction
  | Ast.True ->
    (True, BaseET(mkpos Bool, mkpos Fixed Public))
  | Ast.False ->
    (False, BaseET(mkpos Bool, mkpos Fixed Public))
  | Ast.IntLiteral n ->
    (IntLiteral n, BaseET(mkpos Num(abs n,n < 0), mkpos Fixed Public))
  | Ast.Variable x ->
    let x',xref = Env.find_var tc_ctx.venv x in
      (Variable x', refvt_to_etype' xref)
  | Ast.ArrayGet(x,e) ->
    let x',xref = Env.find_var tc_ctx.venv x in
    let e' = tc_expr tc_ctx e in
      (ArrayGet(x',e'), refvt_to_betype' xref)
  | Ast.ArrayLen x ->
    (* XXX type should be size_t not uint32 *)
    let _,xref = Env.find_var tc_ctx.venv x in
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
    let e' = tc_expr tc_ctx e in
      if not (is_int (expr_to_btype e')) then raise @@ err(e'.pos);
    let ml = expr_to_ml e' in
      (IntCast(b',e'), BaseET(b',ml))
  | Ast.Declassify e ->
    let e' = tc_expr tc_ctx e in
      (Declassify e', BaseET(expr_to_btype e', mkpos Fixed Public))
  | Ast.UnOp(op,e) ->
    let e' = tc_expr tc_ctx e in
      tc_unop' p op e'
  | Ast.BinOp(op,e1,e2) ->
    let e1' = tc_expr tc_ctx e1 in
    let e2' = tc_expr tc_ctx e2 in
      tc_binop' p op e1' e2'
  | Ast.TernOp(e1,e2,e3) ->
    let e1' = tc_expr tc_ctx e1 in
      if not (is_bool (expr_to_btype e1')) then raise @@ err(e1'.pos);
    let e2' = tc_expr tc_ctx e2 in
    let e3' = tc_expr tc_ctx e3 in
      (TernOp(e1',e2',e3'), join_ty' p (type_of e2') (type_of e3'))
  | Ast.FnCall(f,args) ->
    begin
      match (Env.find_var tc_ctx.fenv f).data with
        | (FunDec(_,Some rty,params,_)) ->
          let args' = tc_args true tc_ctx p params args in
            (FnCall(f,args'), rty.data)
        | (CExtern(_,Some rty,params)) ->
          let args' = tc_args false tc_ctx p params args in
            (FnCall(f,args'), rty.data)
    end

and tc_arrayexpr tc_ctx = pfunction
  | Ast.ArrayLit exprs ->
    (* XXX check that all expr types are compatible *)
    let exprs' = List.map (tc_expr tc_ctx) exprs in
    let b = expr_to_btype @@ List.hd exprs' in (* XXX should be join of all exprs' *)
    let at' = mkpos ArrayAT(b, mkpos LIntLiteral(List.length exprs')) in
      (ArrayLit exprs', ArrayET(at', mkpos Fixed Public (* XXX should be join of all exprs' *), mkpos Mut))
  | Ast.ArrayVar x ->
    let x',xref = Env.find_var tc_ctx.venv x in
      (ArrayVar x', refvt_to_etype' xref)
  | Ast.ArrayZeros lexpr ->
    let b = mkpos Num(0, false) in
    let lexpr' = lexprconv lexpr in
    let at' = mkpos ArrayAT(b, lexpr') in
    (ArrayZeros lexpr', ArrayET(at', mkpos Fixed Public, mkpos Mut))
  | Ast.ArrayCopy x ->
    let x',vt = Env.find_var tc_ctx.venv x in
    let ae' = refvt_to_etype' vt in
      (ArrayCopy x', ae')
  | Ast.ArrayView(x,e,lexpr) ->
    let e' = tc_expr tc_ctx e in
    let lexpr' = lexprconv lexpr in
    let x',vt = Env.find_var tc_ctx.venv x in
    let ae = refvt_to_etype vt in
    let ae' = aetype_update_lexpr' lexpr'.data ae in
      (ArrayView(x',e',lexpr'), ae')
  | Ast.ArrayComp(b,lexpr,x,e) ->
    let b' = bconv b in
    let lexpr' = lexprconv lexpr in
    let e' = tc_expr tc_ctx e in
    let ae = ArrayET(mkpos ArrayAT(b', lexpr'), expr_to_ml e', mkpos Mut) in
      (ArrayComp(b',lexpr',x,e'), ae)

let rec tc_stm tc_ctx = pfunction

  | Ast.BaseDec(x,vt,e) ->
    let e' = tc_expr tc_ctx e in
    let ety = type_of e' in
    let vt' = refvt_conv vt in
    let xty = refvt_to_etype vt' in
      if not (ety <:$ xty) then raise @@ err(e'.pos);
      let x' = add_new_var tc_ctx.venv x vt' in
        BaseDec(x',vt',e')

  | Ast.BaseAssign(x,e) ->
    let e' = tc_expr tc_ctx e in
    let x',vt = Env.find_var tc_ctx.venv x in
    let b,{data=Fixed l},m = refvt_type_out vt in
      if m.data <> Mut then raise @@ cerr("variable `" ^ x.data ^ "` is not mut; ", p);
      (* TODO check that types match *)
      if not ((!(tc_ctx.rp) +$. tc_ctx.pc) <$. l) then raise @@ err(p);
      BaseAssign(x',e')

  | Ast.ArrayDec(x,vt,ae) ->
    let ae' = tc_arrayexpr tc_ctx ae in
    let aty = atype_of ae' in
    (* if vt is LUnspecified then take it from aty *)
    let ae_lexpr' = aetype_to_lexpr' aty in
    let vt' = refvt_conv_fill ae_lexpr' vt in
    let xty = refvt_to_etype vt' in
      (* TODO check that types match *)
    let x' = add_new_var tc_ctx.venv x vt' in
      ArrayDec(x',vt',ae')

  | Ast.ArrayAssign(x,n,e) ->
    let n' = tc_expr tc_ctx n in
    let e' = tc_expr tc_ctx e in
    let x',vt = Env.find_var tc_ctx.venv x in
    let b,{data=Fixed l},m = refvt_type_out vt in
      if m.data <> Mut then raise @@ cerr("variable `" ^ x.data ^ "` is not mut; ", p);
      (* TODO check that types match *)
      (* TODO check that n' won't be out-of-bounds *)
      if not ((!(tc_ctx.rp) +$. tc_ctx.pc) <$. l) then raise @@ err(p);
      ArrayAssign(x',n',e')

  | Ast.If(cond,thenstms,elsestms) ->
    let cond' = tc_expr tc_ctx cond in
    let {data=Fixed l} = expr_to_ml cond' in
    (* TODO check that cond' is bool *)
    let pc' = tc_ctx.pc +$. l in
    let tc_ctx1 = { tc_ctx with pc=pc'; rp=ref !(tc_ctx.rp); venv=(Env.sub_env tc_ctx.venv) } in
    let tc_ctx2 = { tc_ctx with pc=pc'; rp=ref !(tc_ctx.rp); venv=(Env.sub_env tc_ctx.venv) } in
    let thenstms' = tc_block tc_ctx1 thenstms in
    let elsestms' = tc_block tc_ctx2 elsestms in
      tc_ctx.rp := !(tc_ctx1.rp) +$. !(tc_ctx2.rp);
      If(cond',thenstms',elsestms')

  | Ast.For(i,ity,lo,hi,stms) ->
    let ity' = bconv ity in
    let lo' = tc_expr tc_ctx lo in
    let hi' = tc_expr tc_ctx hi in
    (* TODO check types and labels *)
    let venv' = Env.sub_env tc_ctx.venv in
    let i' = add_new_var venv' i (mkpos RefVT(ity',mkpos Fixed Public,mkpos Const)) in
      let tc_ctx' = { tc_ctx with venv=venv' } in
      let stms' = tc_block tc_ctx' stms in
        For(i',ity',lo',hi',stms')

  | Ast.VoidFnCall(f,args) ->
    begin
      match (Env.find_var tc_ctx.fenv f).data with
        | (FunDec(_,_,params,_)) ->
          (* TODO ensure no mut args lower than *rp U pc *)
          (* e.g. fcall with public mut arg in a block where pc is Secret is disallowed *)
          let args' = tc_args true tc_ctx p params args in
            VoidFnCall(f,args')
        | (CExtern(_,_,params)) ->
          (* TODO ensure no mut args lower than *rp U pc *)
          (* e.g. fcall with public mut arg in a block where pc is Secret is disallowed *)
          let args' = tc_args false tc_ctx p params args in
            VoidFnCall(f,args')
    end

  | Ast.Return e ->
    let e' = tc_expr tc_ctx e in
      (* TODO check type *)
      tc_ctx.rp := !(tc_ctx.rp) +$. tc_ctx.pc;
      Return e'

  | Ast.VoidReturn ->
    (* TODO check that fn is indeed void *)
    VoidReturn

and tc_block tc_ctx stms =
  let stms' = List.map (tc_stm tc_ctx) stms in
    (tc_ctx.venv, stms')

let tc_param' xf_param = xfunction
  | Ast.Param(x,vty) ->
    let len = "__" ^ x.data ^ "_len" in
    let lexpr' = LDynamic(mkpos len) in
    (* the lexpr will only get used if vty is LUnspecified *)
    let refvt = refvt_conv_fill lexpr' vty in
    let param = Param(x,refvt) in
    let lexpr = refvt_to_lexpr_option refvt in
      param :: (match lexpr with
                 | Some LDynamic len when xf_param ->
                   let lenvt = mkpos RefVT(mkpos UInt 32, mkpos Fixed Public, mkpos Const) in
                     [Param(len, lenvt)]
                 | _ -> [])
let tc_param xf_param pa = List.map (make_ast pa.pos) (tc_param' xf_param pa)

let tc_fdec' fenv = function
  | Ast.FunDec(f,rt,params,stms) ->
    let rt' =
      match rt with
        | Some rty -> Some(etype_conv rty)
        | None -> None
    in
    let params' = List.flatten @@ List.map (tc_param true) params in
    let venv = Env.new_env () in
      List.iter (fun {data=Param(name,vty)} ->
                  let entry = (name,vty) in
                    Env.add_var venv name entry)
        params';
      let tc_ctx = { rp=ref Public; pc=Public; venv; fenv } in
      let stms' = List.rev @@
        match List.rev stms with
        | [] -> [make_ast f.pos Ast.VoidReturn]
        | s::ss ->
          begin
            match s.data with
              | Ast.Return _
              | Ast.VoidReturn -> s::ss
              | _ -> make_ast s.pos Ast.VoidReturn::s::ss
          end
      in
        FunDec(f,rt',params',tc_block tc_ctx stms')
  | Ast.CExtern(f,rt,params) ->
    let rt' =
      match rt with
        | Some rty -> Some(etype_conv rty)
        | None -> None
    in
    let params' = List.flatten @@ List.map (tc_param false) params in
    let venv = Env.new_env () in
      List.iter (fun {data=Param(name,vty)} ->
                  let entry = (name,vty) in
                    Env.add_var venv name entry)
        params';
      let tc_ctx = { rp=ref Public; pc=Public; venv; fenv } in
        CExtern(f,rt',params')
let tc_fdec fenv = xfunction
  | Ast.FunDec(f,_,_,_)
  | Ast.CExtern(f,_,_) as fdec ->
    let fdec' = mkpos tc_fdec' fenv fdec in
      Env.add_var fenv f fdec';
      fdec'

let tc_module (Ast.Module fdecs) =
  let fenv = Env.new_env () in
  let ret = Module (fenv, List.map (tc_fdec fenv) fdecs) in
    (*print_endline (Env.show_env pp_function_dec fenv);*)
    ret
