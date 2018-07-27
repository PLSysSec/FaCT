open Pos
open Err
open Tast
open Tast_utils
open Pseudocode

let mkp ast p = make_ast p ast

type tc_ctx_record = {
  rp       : label' ref;
  pc       : label';
  rt       : ret_type;
  venv     : (var_name * variable_type) Env.env;
  fenv     : (function_dec * bool ref) Env.env;
  sdecs    : struct_type list;
  add_stms : statement' list ref;
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


(* Trivial conversions *)

let bconv =
  wrap @@ fun p -> function
    | Ast.UInt n -> UInt n
    | Ast.Int n -> Int n
    | Ast.Bool -> Bool
    | Ast.UVec(bw,n) -> UVec(bw,n)
    | Ast.String -> raise @@ err p

let mlconv =
  wrap @@ fun p -> function
    | Ast.Public -> Fixed Public
    | Ast.Secret -> Fixed Secret
    | Ast.Unknown -> raise @@ cerr("must specify label for variable", p)

let mconv =
  wrap @@ fun p -> function
    | Ast.Const -> Const
    | Ast.Mut -> Mut

let etype_conv =
  wrap @@ fun p -> function
    | Ast.BaseET(b,l) ->
      BaseET(bconv b, mlconv l)
    | Ast.ArrayET _ -> raise @@ err p

let vattr_conv = function
  | { Ast.cache_aligned; } ->
    { cache_aligned; }

let pattr_conv = function
  | { Ast.output_only; } ->
    { output_only; }

let refvt_conv =
  wrap @@ fun p -> function
    | Ast.RefVT(b,l,m) ->
      RefVT(bconv b, mlconv l, mconv m)
    | Ast.ArrayVT _ -> raise @@ cerr("expected non-array, got array instead", p)
    | Ast.StructVT _ -> raise @@ err p

let fieldtype_conv =
  wrap @@ fun p -> function
    | Ast.RefVT(b,l,m) ->
      RefVT(bconv b, mlconv l, mconv m)
    | Ast.ArrayVT(a,ml,m,attr) ->
      let aconv =
        wrap @@ fun p -> function
          | Ast.ArrayAT(bt,le) ->
            let leconv =
              wrap @@ fun p -> function
                | Ast.LExpression ({data=Ast.IntLiteral n}) ->
                  LIntLiteral n
                | _ -> raise @@ err p in
              ArrayAT(bconv bt, leconv le) in
        ArrayVT(aconv a, mlconv ml, mconv m, vattr_conv attr)
    | Ast.StructVT(s,m) ->
      StructVT(s, mconv m)

let inline_conv = function
  | Ast.Default -> Default
  | Ast.Always -> Always
  | Ast.Never -> Never

let fntype_conv ft =
  { export=ft.Ast.export; inline=inline_conv ft.Ast.inline }



(* Actual typechecking *)

let tc_unop' p op e =
  let b,ml = expr_to_types e in
    begin
      match op with
        | Ast.Neg ->
          if not (is_int b) then raise @@ cerr("operator expects an integer type", p);
        | Ast.BitwiseNot ->
          if not (is_int b) then raise @@ cerr("operator expects an integer type", p);
        | Ast.LogicalNot ->
          if not (is_bool b) then raise @@ cerr("operator expects a boolean type", p);
    end;
    begin
      match b.data with
        | Num(k,s) ->
          let n = k * if s then -1 else 1 in
            begin
              match op with
                | Ast.Neg        -> make_nlit p ( -n)
                | Ast.BitwiseNot -> make_nlit p ((-n) - 1)
            end
        | _ ->
          (UnOp(op, e), BaseET(b, ml))
    end

let tc_binop_check p op b1 b2 =
  match op with
    | Ast.Equal
    | Ast.NEqual ->
      if (is_bool b1 && not (is_bool b2))
      || (is_bool b2 && not (is_bool b1))
      then raise @@ cerr("cannot compare boolean and integer", p)
    | Ast.Plus
    | Ast.Minus
    | Ast.Multiply
    | Ast.BitwiseAnd
    | Ast.BitwiseOr
    | Ast.BitwiseXor
    | Ast.LeftShift
    | Ast.RightShift
    | Ast.LeftRotate
    | Ast.RightRotate ->
      if not (is_int b1) || not (is_int b2) then raise @@ cerr("operands must be numeric", p)
    | Ast.Divide
    | Ast.Modulo
    | Ast.GT
    | Ast.GTE
    | Ast.LT
    | Ast.LTE ->
      if not (is_int b1) || not (is_int b2) then raise @@ cerr("operands must be numeric", p);
      if not (joinable_bt b1 b2) then raise @@ cerr(Printf.sprintf
                                                      "types are not compatible: %s and %s"
                                                      (ps_bty b1)
                                                      (ps_bty b2), p)
    | Ast.LogicalAnd
    | Ast.LogicalOr ->
      if not (is_bool b1) || not (is_bool b2) then raise @@ cerr("operands must be boolean", p)

let tc_binop' p op e1 e2 =
  let b1,ml1 = expr_to_types e1 in
  let b2,ml2 = expr_to_types e2 in
    match b1.data, b2.data with
      | Num (k1,s1), Num (k2,s2) ->
        let n = k1 * if s1 then -1 else 1 in
        let m = k2 * if s2 then -1 else 1 in
          begin
            match op with
              | Ast.Plus        -> make_nlit p (n + m)
              | Ast.Minus       -> make_nlit p (n - m)
              | Ast.Multiply    -> make_nlit p (n * m)
              | Ast.Divide      -> make_nlit p (n / m)
              | Ast.Modulo      -> make_nlit p (n mod m)
              | Ast.BitwiseOr   -> make_nlit p (n lor m)
              | Ast.BitwiseXor  -> make_nlit p (n lxor m)
              | Ast.BitwiseAnd  -> make_nlit p (n land m)
              | Ast.Equal       -> make_blit p (n = m)
              | Ast.NEqual      -> make_blit p (n <> m)
              | Ast.GT          -> make_blit p (n > m)
              | Ast.GTE         -> make_blit p (n >= m)
              | Ast.LT          -> make_blit p (n < m)
              | Ast.LTE         -> make_blit p (n <= m)
              | Ast.LeftShift   -> make_nlit p (n lsl m)
              | Ast.RightShift  -> make_nlit p (n asr m)
              | Ast.LeftRotate
              | Ast.RightRotate -> raise @@ cerr("can't bitwise rotate constants of unknown size", p)
          end
      | _ ->
        tc_binop_check p op b1 b2;
        let b' =
          match op with
            | Ast.Plus
            | Ast.Minus
            | Ast.Multiply
            | Ast.BitwiseOr
            | Ast.BitwiseXor
            | Ast.LogicalAnd
            | Ast.LogicalOr -> join_bt p b1 b2

            | Ast.Divide
            | Ast.Modulo ->
              if (ml1.data = Fixed Secret) || (ml2.data = Fixed Secret) then
                raise @@ cerr("cannot use secrets in division/modulo", p)
              else
                join_bt p b1 b2

            | Ast.BitwiseAnd -> min_bt p b1 b2

            | Ast.Equal
            | Ast.NEqual
            | Ast.GT
            | Ast.GTE
            | Ast.LT
            | Ast.LTE -> (mkp Bool p)

            | Ast.LeftShift
            | Ast.RightShift
            | Ast.LeftRotate
            | Ast.RightRotate -> { b1 with pos=p }
        in
        let ml' = join_ml p ml1 ml2 in
          (BinOp(op, e1, e2), BaseET(b', ml'))

let params_all_refs_above tc_ctx rpc params =
  let rec checker n = function
    | [] -> -1
    | ({data=Param(_,{data=vty'},_); pos=p}::params) ->
      begin
        match vty' with
          | RefVT(_,{data=Fixed l},{data=mut})
          | ArrayVT(_,{data=Fixed l},{data=mut},_) ->
            if (mut <> Mut) || (rpc <$. l)
            then (checker (n+1) params)
            else n
          | StructVT(s,{data=mut}) ->
            if (rpc = Public) || (mut <> Mut) || (not @@ struct_has_secrets tc_ctx.sdecs s)
            then (checker (n+1) params)
            else n
      end
  in
    checker 0 params

let rec lexprconv tc_ctx =
  wrap @@ fun p -> function
    | Ast.LExpression ({data=Ast.IntLiteral n}) ->
      LIntLiteral n
    | Ast.LExpression e ->
      let lenvt = (mkp (RefVT((mkp (UInt 32) p), (mkp (Fixed Public) p), (mkp Const p))) p) in
      let e' = tc_expr tc_ctx e in
      let Fixed ml = (expr_to_ml e').data in
        if not (ml = Public) then raise @@ cerr("Array length expressions must be public", p);
        let len_var = (mkp (make_fresh "len") p) in
        let len = add_new_var tc_ctx.venv len_var lenvt in
          tc_ctx.add_stms := (BaseDec(len,lenvt,e')) :: !(tc_ctx.add_stms);
          LDynamic len
    | Ast.LUnspecified -> raise @@ err(p) (* should never get here *)

and atype_conv_fill tc_ctx lexpr' =
  wrap @@ fun p -> function
    | Ast.ArrayAT(bt,{data=Ast.LUnspecified}) ->
      ArrayAT(bconv bt, (mkp lexpr' p))
    | Ast.ArrayAT(bt,le) ->
      ArrayAT(bconv bt, lexprconv tc_ctx le)

and refvt_conv_fill tc_ctx lexpr' =
  wrap @@ fun p -> function
    | Ast.RefVT(b,l,m) ->
      RefVT(bconv b, mlconv l, mconv m)
    | Ast.ArrayVT(a,ml,m,attr) ->
      ArrayVT(atype_conv_fill tc_ctx lexpr' a, mlconv ml, mconv m, vattr_conv attr)
    | Ast.StructVT(s,m) ->
      if not (has_struct tc_ctx.sdecs s) then raise @@ cerr("struct " ^ s.data ^ " does not exist", p);
      StructVT(s,mconv m)

and tc_arg tc_ctx =
  wrap @@ fun p -> function
    | Ast.ByValue e ->
      begin
        match e.data with
          | Ast.Lvalue lval ->
            let _,vty = (tc_lvalue tc_ctx lval).data in
              begin
                match vty with
                  | RefVT _ -> ByValue (tc_expr tc_ctx e)
                  | ArrayVT _ ->
                    let ae',_ = tc_arrayexpr tc_ctx (mkp (Ast.ArrayVar lval) p) in
                      ByArray(ae', (mkp Const p))
                  | _ -> raise @@ cerr("somethng wetn worng", p)
              end
          | _ -> ByValue (tc_expr tc_ctx e)
      end
    | Ast.ByRef x ->
      let x' = tc_lvalue tc_ctx x in
      let (_,xref) = x'.data in
        begin
          match xref with
            | RefVT _ -> ByRef x'
            | ArrayVT _ ->
              let ae',_ = tc_arrayexpr tc_ctx (mkp (Ast.ArrayVar x) p) in
              let (_,ArrayET(_,_,mut)) = ae'.data in
                if not (mut.data <* Mut) then raise @@ cerr("variable is not mut", p);
                ByArray(ae', (mkp Mut p))
            | StructVT _ -> ByRef x'
        end
    | Ast.ByArray(arr_expr, mutability) ->
      let m' = mconv mutability in
      let ae',_ = tc_arrayexpr tc_ctx arr_expr in
      let (_,ArrayET(_,_,mut)) = ae'.data in
        if not (mut.data <* m'.data) then raise @@ cerr("array expression is not proper mutability", p);
        ByArray(ae', m')

and argtype_of tc_ctx =
  xwrap @@ fun p -> function
    | ByValue e ->
      let b,ml = expr_to_types e in
        (mkp (RefVT(b,ml,(mkp Const p))) p)
    | ByRef lval ->
      let _,vt = lval.data in
        (mkp vt p)
    | ByArray({data=(aexpr,aty)}, mut) ->
      let b,ml = atype_out (mkp aty p) in
        (mkp (ArrayVT(b,ml,mut,default_var_attr)) p)

and tc_args ~xf_args tc_ctx p params args =
  match params,args with
    | [], [] -> []
    | (param::params), (arg::args) ->
      let arg' = tc_arg tc_ctx arg in
      let argref = argtype_of tc_ctx.venv arg' in
      let Param(_,paramvt,_) = param.data in
        check_can_be_passed_to argref paramvt;
        if param_is_ldynamic param && xf_args then
          let _::params = params in
          let ByArray({data=(_,atype')},_) = arg'.data in
          let lexpr' = aetype_to_lexpr' (mkp atype' p) in
          let len =
            match lexpr' with
              | LIntLiteral n ->
                ByValue (mkp (IntLiteral n, BaseET(mkp (Num(abs n,n < 0)) p, (mkp (Fixed Public) p))) p)
              | LDynamic lx ->
                let vt = RefVT(mkp (UInt 32) p, mkp (Fixed Public) p, mkp Const p) in
                  ByValue (mkp (Lvalue(mkp (Base lx, vt) p), BaseET(mkp (UInt 32) p, mkp (Fixed Public) p)) p)
          in
            arg' :: (mkp len p) :: tc_args ~xf_args tc_ctx p params args
        else
          arg' :: tc_args ~xf_args tc_ctx p params args
    | _ -> raise @@ cerr("mismatch in args vs params length", p)

and tc_lvalue tc_ctx =
  wrap @@ fun p -> function
    | Ast.Base x ->
      let x',vt = Env.find_var tc_ctx.venv x in
        Base(x'), vt.data
    | Ast.ArrayEl(lval,n) ->
      let lval' = tc_lvalue tc_ctx lval in
      let (_,vt) = lval'.data in
      let n' = tc_expr tc_ctx n in
      let Fixed ml = (expr_to_ml n').data in
        if not (ml = Public) then raise @@ cerr("Array indices must be public", p);
        ArrayEl(lval', tc_expr tc_ctx n), (arrayvt_to_refvt (mkp vt p)).data
    | Ast.StructEl(lval,field) ->
      let lval' = tc_lvalue tc_ctx lval in
      let (_,vt) = lval'.data in
      let StructVT(s,m) = vt in
      let Struct(_,fields) = (find_struct tc_ctx.sdecs s).data in
      let Field(_,fvt,_) =
        try (List.find (fun {data=Field(fn,_,_)} -> field.data = fn.data) fields).data
        with
          | Not_found -> raise @@ cerr(Printf.sprintf "Unknown field: `%s`" field.data, field.pos)
      in
      let fvt' = refvt_update_mut' (refvt_mut_out' vt) fvt in
        StructEl(lval', field), fvt'

and tc_expr tc_ctx =
  wrap @@ fun p -> function
    | Ast.True ->
      make_blit p true
    | Ast.False ->
      make_blit p false
    | Ast.IntLiteral n ->
      make_nlit p n
    | Ast.StringLiteral s ->
      (StringLiteral s, BaseET(mkp String p, make_ast p @@ Fixed Public))
    | Ast.Lvalue lval ->
      let lval' = tc_lvalue tc_ctx lval in
      let (_,vt) = lval'.data in
        (Lvalue lval', refvt_to_etype' (p @> vt))
    | Ast.ArrayElLen lval ->
      (* XXX type should be size_t not uint32 *)
      let _,vt = (tc_lvalue tc_ctx lval).data in
      let lexpr = refvt_to_lexpr (p @> vt) in
        begin
          match lexpr.data with
            | LIntLiteral n ->
              (IntLiteral n, BaseET(p @> Num(abs n,n < 0), p @> Fixed Public))
            | LDynamic len ->
              let lenvt = RefVT(p @> UInt 32, p @> Fixed Public, p @> Const) in
                (Lvalue(p @> (Base len,lenvt)), BaseET(p @> UInt 32, p @> Fixed Public))
        end
    | Ast.IntCast(b,e) ->
      let b' = bconv b in
        if not (is_int b') then raise @@ cerr("only integer casts are allowed", b'.pos);
        let e' = tc_expr tc_ctx e in
          if not (is_int (expr_to_btype e')) then raise @@ cerr("only integers can be casted", e'.pos);
          let ml = expr_to_ml e' in
            (IntCast(b',e'), BaseET(b',ml))
    | Ast.Declassify e ->
      let e' = tc_expr tc_ctx e in
        (Declassify e', BaseET(expr_to_btype e', p @> Fixed Public))
    | Ast.UnOp(op,e) ->
      let e' = tc_expr tc_ctx e in
        tc_unop' p op e'
    | Ast.BinOp(op,e1,e2) ->
      let e1' = tc_expr tc_ctx e1 in
      let e2' = tc_expr tc_ctx e2 in
        tc_binop' p op e1' e2'
    | Ast.TernOp(e1,e2,e3) ->
      let e1' = tc_expr tc_ctx e1 in
        if not (is_bool (expr_to_btype e1')) then raise @@ cerr("expected a boolean", e1'.pos);
        let ml1 = expr_to_ml e1' in
        let e2' = tc_expr tc_ctx e2 in
        let e3' = tc_expr tc_ctx e3 in
        let BaseET(b',ml') = join_ty' p (type_of e2') (type_of e3') in
        let bty' = BaseET(b',join_ml p ml1 ml') in
          (TernOp(e1',e2',e3'), bty')
    | Ast.FnCall(f,args) ->
      let rpc = !(tc_ctx.rp) +$. tc_ctx.pc in
      let (fdec,everhi) =  Env.find_var tc_ctx.fenv f in
        if rpc = Secret then everhi := true;
        begin
          match fdec.data with
            | (FunDec(_,fty,Some rty,params,_))
            | (StdlibFunDec(_,fty,Some rty,params)) ->
              if (!everhi) && fty.export then raise @@ cerr("Cannot call exported function from a secret context", p);
              (* ensure no mut args lower than rp U pc *)
              (* e.g. fcall with public mut arg in a block where pc is Secret *)
              let earg_n = params_all_refs_above tc_ctx rpc params in
                if earg_n >= 0 then
                  (let earg = List.nth args earg_n in
                     raise @@ cerr("cannot call function with public mutable arg while in a secret context", earg.pos));
                let args' = tc_args ~xf_args:true tc_ctx p params args in
                  (FnCall(f,args'), rty.data)
            | (CExtern(_,Some rty,params)) ->
              (* ensure no mut args lower than rp U pc *)
              (* e.g. fcall with public mut arg in a block where pc is Secret *)
              let earg_n = params_all_refs_above tc_ctx rpc params in
                if earg_n >= 0 then
                  (let earg = List.nth args earg_n in
                     raise @@ cerr("cannot call function with public mutable arg while in a secret context", earg.pos));
                let args' = tc_args ~xf_args:false tc_ctx p params args in
                  (FnCall(f,args'), rty.data)
            | (DebugFunDec(_,Some rty,params)) ->
              let args' = tc_args ~xf_args:false tc_ctx p params args in
                DebugFnCall(f,args'), rty.data
        end
    | Ast.Shuffle(e,mask) ->
      let e' = tc_expr tc_ctx e in
      let BaseET(b, ml) = type_of' e' in
      let UVec(bw, n) = b.data in
      let len = List.length mask in
      let b' =
        if len = 1 then
          UInt bw
        else
          UVec(bw, len) in
        Shuffle(e',mask), BaseET(p @> b', ml)

(* returns ((Tast.array_expr', Tast.ArrayET), is_new_memory) *)
and tc_arrayexpr' tc_ctx =
  xwrap @@ fun p -> function
    | Ast.ArrayLit exprs ->
      (* XXX check that all expr types are compatible *)
      let exprs' = List.map (tc_expr tc_ctx) exprs in
      let b = expr_to_btype @@ List.hd exprs' in (* XXX should be join of all exprs' *)
      let at' = p @> ArrayAT(b, p @> LIntLiteral(List.length exprs')) in
        (ArrayLit exprs',
         ArrayET(at', p @> Fixed Public (* XXX should be join of all exprs' *), p @> Const)),
        true
    | Ast.ArrayVar x ->
      let x' = tc_lvalue tc_ctx x in
      let (_,xref) = x'.data in
        (ArrayVar x', refvt_to_etype' (p @> xref)), false
    | Ast.ArrayZeros lexpr ->
      let b = p @> Num(0, false) in
      let lexpr' = lexprconv tc_ctx lexpr in
      let at' = p @> ArrayAT(b, lexpr') in
        (ArrayZeros lexpr', ArrayET(at', p @> Fixed Public, p @> Const)), true
    | Ast.ArrayCopy x ->
      let x' = tc_lvalue tc_ctx x in
      let (_,vt) = x'.data in
      let ae' = refvt_to_etype' (p @> vt) in
        (ArrayCopy x', aetype_update_mut' (p @> Mut) ae'), true
    | Ast.ArrayView(x,e,lexpr) ->
      let e' = tc_expr tc_ctx e in
      let Fixed ml = (expr_to_ml e').data in
        if not (ml = Public) then raise @@ cerr("arrayview index must be public", p);
        let lexpr' = lexprconv tc_ctx lexpr in
        let x' = tc_lvalue tc_ctx x in
        let (_,vt) = x'.data in
        let ae = refvt_to_etype (p @> vt) in
        let ae' = aetype_update_lexpr' lexpr'.data ae in
          (ArrayView(x',e',lexpr'), ae'), false
    | Ast.ArrayComp(b,lexpr,x,e) ->
      let b' = bconv b in
      let lexpr' = lexprconv tc_ctx lexpr in
      let e' = tc_expr tc_ctx e in
      let ae = ArrayET(p @> ArrayAT(b', lexpr'), expr_to_ml e', p @> Mut) in
        (ArrayComp(b',lexpr',x,e'), ae), true
    | Ast.ArrayNoinit lexpr ->
      let b = p @> Num(0, false) in
      let lexpr' = lexprconv tc_ctx lexpr in
      let at' = p @> ArrayAT(b, lexpr') in
        (ArrayNoinit lexpr', ArrayET(at', p @> Fixed Public, p @> Const)), true
and tc_arrayexpr tc_ctx pa =
  let ae', is_mem_new = tc_arrayexpr' tc_ctx pa in
    make_ast pa.pos ae', is_mem_new

let rec tc_stm' tc_ctx =
  xwrap @@ fun p -> function

    | Ast.BaseDec(x,vt,e) ->
      let e' = tc_expr tc_ctx e in
      let ety = type_of e' in
      let vt' = refvt_conv vt in
      let xty = refvt_to_etype vt' in
        if not (ety <:$ xty) then
          raise @@ cerr("expression of type `" ^ ps_ety ety ^ "` cannot be assigned to variable of type `" ^ ps_ety xty ^ "`", p);
        let x' = add_new_var tc_ctx.venv x vt' in
          [BaseDec(x',vt',e')]

    | Ast.ArrayDec(x,vt,ae) ->
      let ae',is_new_mem = tc_arrayexpr tc_ctx ae in
      let aty = atype_of ae' in
      (* if vt is LUnspecified then take it from aty *)
      let ae_lexpr' = aetype_to_lexpr' aty in
      let vt' = refvt_conv_fill tc_ctx ae_lexpr' vt in
      let xty = refvt_to_etype vt' in
        if not ((aty,is_new_mem) <:$* xty) then
          raise @@ cerr("array of type `" ^ ps_ety aty ^ "` cannot be assigned to variable of type `" ^ ps_ety xty ^ "`", p);

        let x' = add_new_var tc_ctx.venv x vt' in
          [ArrayDec(x',vt',ae')]

    | Ast.StructDec(x,s) -> (* XXX make this better *)
      let svt = p @> StructVT(s, p @> Mut) in
      let x' = add_new_var tc_ctx.venv x svt in
        [StructDec(x',svt)]

    | Ast.Assign(lval,e) ->
      let lval' = tc_lvalue tc_ctx lval in
      let (_,vt) = lval'.data in
      let e' = tc_expr tc_ctx e in
      let b,{data=Fixed l},m = refvt_type_out (p @> vt) in
        (* check that x is indeed mutable *)
        if m.data <> Mut then raise @@ cerr("variable is not mutable", p);

        (* check that rp U pc is <= label of x *)
        if not ((!(tc_ctx.rp) +$. tc_ctx.pc) <$. l) then
          raise @@ cerr("cannot assign to " ^ ps_label' p l ^ " variable when program context is " ^ ps_label' p (!(tc_ctx.rp) +$. tc_ctx.pc), p);

        (* check that labeled type of e is <= labeled type of x *)
        let ety = type_of e' in
        let xty = refvt_to_etype (p @> vt) in
        let BaseET(bty1,l1) = ety.data in
        let BaseET(bty2,l2) = xty.data in
          if not (bty1 <: bty2) then
            raise @@ cerr("expression of type `" ^ ps_bty bty1 ^ "` cannot be assigned to variable of type `" ^ ps_bty bty2 ^ "`", p);
          if not (l1 <$ l2) then
            raise @@ cerr("expression with label `" ^ ps_label l1 ^ "` cannot be assigned to variable with label`" ^ ps_label l2 ^ "`", p);

          [Assign(lval',e')]

    | Ast.If(cond,thenstms,elsestms) ->
      let cond' = tc_expr tc_ctx cond in
      let {data=Fixed l} = expr_to_ml cond' in

      (* check that labeled type of cond is <= secret bool *)
      let condty = type_of cond' in
      let ifty = p @> BaseET(p @> Bool, p @> Fixed Secret) in
        if not (condty <:$ ifty) then
          raise @@ cerr("expression of type `" ^ ps_ety condty ^ "` cannot be used as a conditional", p);

        let pc' = tc_ctx.pc +$. l in
        let tc_ctx1 = { tc_ctx with pc=pc'; rp=ref !(tc_ctx.rp); venv=(Env.sub_env tc_ctx.venv) } in
        let tc_ctx2 = { tc_ctx with pc=pc'; rp=ref !(tc_ctx.rp); venv=(Env.sub_env tc_ctx.venv) } in
        let thenstms' = tc_block tc_ctx1 thenstms in
        let elsestms' = tc_block tc_ctx2 elsestms in
          tc_ctx.rp := !(tc_ctx1.rp) +$. !(tc_ctx2.rp);
          [If(cond',thenstms',elsestms')]

    | Ast.For(i,ity,init,cond,upd_stmt,stms) ->
      let ity' = bconv ity in

      let venv' = Env.sub_env tc_ctx.venv in
      let i' = add_new_var venv' i (p @> RefVT(ity',p @> Fixed Public,p @> Const)) in
      let tc_ctx' = { tc_ctx with venv=venv' } in

      (* special tc_ctx'' so that typechecker doesn't barf on the assignment to a "const" *)
      let venv'' = Env.sub_env tc_ctx.venv in
      let vt'' = (p @> RefVT(ity',p @> Fixed Public,p @> Mut)) in
      let entry'' = (i', vt'') in
        Env.add_var venv'' i entry'';
        Env.add_var venv'' i' entry'';
        let tc_ctx'' = { tc_ctx with venv=venv''; rp=ref Public; pc=Public } in

        (* hacky way of checking that init is okay *)
        let assign = make_ast init.pos @@ Ast.Assign(make_ast i.pos @@ Ast.Base i, init) in
          tc_stm tc_ctx'' assign |> ignore;

          let init' = tc_expr tc_ctx init in
          let cond' = tc_expr tc_ctx' cond in

          (* check that labeled type of cond is <= public bool *)
          let condty = type_of cond' in
          let ifty = p @> BaseET(p @> Bool, p @> Fixed Public) in
            if not (condty <:$ ifty) then
              raise @@ cerr("expression of type `" ^ ps_ety condty ^ "` cannot be used as a conditional", p);

            let upd_stmt' = tc_stm tc_ctx'' upd_stmt in
            let upd' =
              match upd_stmt' with
                | [{data=Assign(lval, expr)}] ->
                  begin match lval.data with
                    | Base x,_ ->
                      if x.data <> i'.data then raise @@ cerr("for loop update must use loop iteration variable", upd_stmt.pos);
                      expr
                    | _ -> raise @@ cerr("for loop update must use loop iteration variable", upd_stmt.pos)
                  end
                | _ -> raise @@ cerr("for loop update can be simple assignment only", upd_stmt.pos)
            in

            let stms' = tc_block tc_ctx' stms in (* yes, this needs tc_ctx' and not tc_ctx'' *)
              [For(i',ity',init',cond',upd',stms')]

    | Ast.VoidFnCall(f,args) ->
      let rpc = !(tc_ctx.rp) +$. tc_ctx.pc in
      let (fdec,everhi) =
        Env.find_var'
          (fun v -> v)
          (fun name ->
             let res = Stdlib.get_stdlib_proto name in
               Env.add_var tc_ctx.fenv name res;
               res)
          tc_ctx.fenv f in
        if rpc = Secret then everhi := true;
        begin
          match fdec.data with
            | (FunDec(_,fty,_,params,_))
            | (StdlibFunDec(_,fty,_,params)) ->
              if (!everhi) && fty.export then raise @@ cerr("cannot call exported function from a secret context", p);
              (* ensure no mut args lower than rp U pc *)
              (* e.g. fcall with public mut arg in a block where pc is Secret *)
              let earg_n = params_all_refs_above tc_ctx rpc params in
                if earg_n >= 0 then
                  (let earg = List.nth args earg_n in
                     raise @@ cerr("cannot call function with public mutable arg while in a secret context", earg.pos));

                let args' = tc_args ~xf_args:true tc_ctx p params args in
                  [VoidFnCall(f,args')]
            | (CExtern(_,_,params)) ->
              (* ensure no mut args lower than rp U pc *)
              (* e.g. fcall with public mut arg in a block where pc is Secret *)
              let earg_n = params_all_refs_above tc_ctx rpc params in
                if earg_n >= 0 then
                  (let earg = List.nth args earg_n in
                     raise @@ cerr("cannot call function with public mutable arg while in a secret context", earg.pos));
                let args' = tc_args ~xf_args:false tc_ctx p params args in
                  [VoidFnCall(f,args')]
            | (DebugFunDec(_,_,params)) ->
              let args' = tc_args ~xf_args:false tc_ctx p params args in
                [DebugVoidFnCall(f,args')]
        end

    | Ast.Return e ->
      let e' = tc_expr tc_ctx e in
      let ety = type_of e' in
        begin
          match tc_ctx.rt with
            | None -> raise @@ cerr("cannot return value from a void function", p)
            | Some rty ->
              let BaseET(bty1,l1) = ety.data in
              let BaseET(bty2,l2) = rty.data in
                if not (bty1 <: bty2) then
                  raise @@ cerr("expression of type `" ^ ps_bty bty1 ^ "` cannot be returned from function with return type `" ^ ps_bty bty2 ^ "`", p);
                if not (l1 <$ l2) then
                  raise @@ cerr("expression with label `" ^ ps_label l1 ^ "` cannot be returned from function with label `" ^ ps_label l2 ^ "`", p);
        end;
        tc_ctx.rp := !(tc_ctx.rp) +$. tc_ctx.pc;
        [Return e']

    | Ast.VoidReturn ->
      if tc_ctx.rt <> None then raise @@ cerr("function must return a value", p);
      tc_ctx.rp := !(tc_ctx.rp) +$. tc_ctx.pc;
      [VoidReturn]

and tc_stm tc_ctx pa =
  List.map
    (make_ast pa.pos)
    (let stms = tc_stm' tc_ctx pa in
     let stms' = (List.rev !(tc_ctx.add_stms)) @ stms in
       tc_ctx.add_stms := [];
       stms')

and tc_block tc_ctx stms =
  let stms' = List.flatten @@ List.map (tc_stm tc_ctx) stms in
    (tc_ctx.venv, stms')

let tc_param' sdecs xf_param =
  xwrap @@ fun p -> function
    | Ast.Param(x,vty,attr) ->
      let len = "__" ^ x.data ^ "_len" in
      let lexpr' = LDynamic(p @> len) in
      (* the lexpr will only get used if vty is LUnspecified *)
      (* XXX the following line is a total hack *)
      let fake_hacky_useless_tc_ctx = {
        rp=ref Public;
        pc=Public;
        rt=None;
        venv=Env.new_env ();
        fenv=Env.new_env ();
        sdecs=sdecs;
        add_stms=ref [];
      } in
      let refvt = refvt_conv_fill fake_hacky_useless_tc_ctx lexpr' vty in
      let param = Param(x,refvt,pattr_conv attr) in
      let lexpr = refvt_to_lexpr_option refvt in
        param :: (match lexpr with
                   | Some LDynamic len when xf_param ->
                     let lenvt = p @> RefVT(p @> UInt 32, p @> Fixed Public, p @> Const) in
                       [Param(len, lenvt, default_param_attr)]
                   | _ -> [])
let tc_param sdecs xf_param pa = List.map (make_ast pa.pos) (tc_param' sdecs xf_param pa)

let tc_fdec' fpos fenv sdecs = function
  | Ast.FunDec(f,ft,rt,params,stms) ->
    let ft' = fntype_conv ft in
    let rt' =
      match rt with
        | Some rty -> Some(etype_conv rty)
        | None -> None
    in
    let params' = List.flatten @@ List.map (tc_param sdecs true) params in
    let venv = Env.new_env () in
      List.iter (fun {data=Param(name,vty,_)} ->
                  let entry = (name,vty) in
                    Env.add_var venv name entry)
        params';
      let tc_ctx = {
        rp=ref Public;
        pc=Public;
        rt=rt';
        venv;
        fenv;
        sdecs;
        add_stms=ref [];
      } in
      let rec final_stmt_rets stms =
        begin
          match List.rev stms with
            | [] -> false
            | s::ss ->
              begin
                match s.data with
                  | Ast.Return _
                  | Ast.VoidReturn -> true
                  | Ast.For(_,_,_,_,_,fstms) ->
                    final_stmt_rets fstms
                  | Ast.If(_,tstms,fstms) ->
                    (final_stmt_rets tstms) && (final_stmt_rets fstms)
                  | _ -> false
              end
        end in
      let stms' = if not (final_stmt_rets stms)
        then stms @ [make_ast fpos Ast.VoidReturn]
        else stms
      in
        FunDec(f,ft',rt',params',tc_block tc_ctx stms')
  | Ast.CExtern(f,rt,params) ->
    let rt' =
      match rt with
        | Some rty -> Some(etype_conv rty)
        | None -> None
    in
    let params' = List.flatten @@ List.map (tc_param sdecs false) params in
    let venv = Env.new_env () in
      List.iter (fun {data=Param(name,vty,_)} ->
                  let entry = (name,vty) in
                    Env.add_var venv name entry)
        params';
      CExtern(f,rt',params')
let tc_fdec fenv sdecs =
  xwrap @@ fun p -> function
    | Ast.FunDec(f,_,_,_,_)
    | Ast.CExtern(f,_,_) as fdec ->
      let fdec' = p @> tc_fdec' p fenv sdecs fdec in
        Env.add_var fenv f (fdec', ref false);
        fdec'

let tc_field =
  wrap @@ fun p -> function
    | Ast.Field(x,vty,is_pointer) ->
      let refvt = fieldtype_conv vty in
        Field(x,refvt,is_pointer)

let tc_sdec =
  xwrap @@ fun p -> function
    | Ast.Struct(s,fields) -> p @> Struct(s,List.map tc_field fields)

let tc_module (Ast.Module (fdecs,sdecs)) =
  let sdecs' = List.map tc_sdec sdecs in
  let fenv = Env.new_env () in
  let add_fenv = (fun (name,fdec) -> Env.add_var fenv name (fdec, ref false)) in
    List.iter add_fenv Debugfun.functions;
    List.iter add_fenv Stdlib.functions;
    let ret = Module (fenv,
                      List.map (tc_fdec fenv sdecs') fdecs,
                      sdecs') in
      ret
