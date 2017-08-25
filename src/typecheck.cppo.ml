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



let z3e = Stack.create ()
let z3_push e = Stack.push e z3e
#define z3_pop (Stack.pop z3e)
#define z3_top (Stack.top z3e)

let z3_bty = xfunction
  | Int n -> (Z.bitvec n,true)
  | UInt n -> (Z.bitvec n,false)
  | Num(_,s) -> (Z.int,s)
  | Bool -> (Z.bool,false)

let z3_ty = xfunction
  | BaseET(b,_) ->
    z3_bty b

let z3_unop = function
  | Ast.Neg -> Z.neg
  | Ast.BitwiseNot -> Z.(!.)
  | Ast.LogicalNot -> Z.(!)


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


(* Extraction *)

let type_of = xfunction
  | (_,ty) -> mkpos ty

let type_out = xfunction
  | BaseET(b,ml) -> (b,ml)

let expr_to_btype = xfunction
  | (_,BaseET(b,_)) -> b

let expr_to_ml = xfunction
  | (_,BaseET(_,ml)) -> ml

let expr_to_types = xfunction
  | (_,BaseET(b,ml)) -> b,ml

let refvt_to_etype' = xfunction
  | RefVT(b,ml,_) -> BaseET(b, ml)
let refvt_to_etype = rebind refvt_to_etype'


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
          begin
            let z3expr = z3_top in
            if is_signed b &&
               Z.is_bv z3expr then
              let [a] = Z.get_args z3expr in
                Z.add_neg_overflow_check a;
          end
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
  let b = z3_pop in
  let a = z3_pop in
  let is_bv = Z.is_bv a || Z.is_bv b in
  let signed = (is_signed b1 || is_signed b2) in
  let b',z3_op =
    match op with
      | Ast.Plus ->
        if is_bv then
          Z.add_add_overflow_check a b signed;
        join_bt p b1 b2, Z.(+)
      | Ast.Minus ->
        if is_bv then
          Z.add_sub_overflow_check a b signed;
        join_bt p b1 b2, Z.(-)
      | Ast.Multiply ->
        if is_bv then
          Z.add_mul_overflow_check a b signed;
        join_bt p b1 b2, Z.( * )
      | Ast.GT ->
        let zop =
          if is_bv then
            if signed then Z.sgt else Z.ugt
          else Z.(>) in
        join_bt p b1 b2, zop
      | Ast.GTE ->
        let zop =
          if is_bv then
            if signed then Z.sge else Z.uge
          else Z.(>=) in
        join_bt p b1 b2, zop
      | Ast.LT ->
        let zop =
          if is_bv then
            if signed then Z.slt else Z.ult
          else Z.(<) in
        join_bt p b1 b2, zop
      | Ast.LTE ->
        let zop =
          if is_bv then
            if signed then Z.sle else Z.ule
          else Z.(<=) in
        join_bt p b1 b2, zop
      | Ast.BitwiseOr ->
        join_bt p b1 b2, Z.(|.)
      | Ast.BitwiseXor ->
        join_bt p b1 b2, Z.(^.)
      | Ast.BitwiseAnd ->
        meet_bt p b1 b2, Z.(&.)
      | Ast.LogicalAnd ->
        join_bt p b1 b2, Z.(&&)
      | Ast.LogicalOr ->
        join_bt p b1 b2, Z.(||)
      | Ast.Equal ->
        join_bt p b1 b2, Z.(=)
      | Ast.NEqual ->
        join_bt p b1 b2, Z.(!=)
      | Ast.LeftShift ->
        { b1 with pos=p }, Z.(<<)
      | Ast.RightShift ->
        let zop =
          if signed then Z.(>>) else Z.(>>.)
        in
          { b1 with pos=p }, zop
  in
    z3_push @@ z3_op a b;
  let ml' = join_ml p ml1 ml2 in
    (BinOp(op, e1, e2), BaseET(b', ml'))

let rec tc_arg fenv venv = pfunction
  | Ast.ByValue e ->
    let arg' = ByValue (tc_expr fenv venv e) in
      z3_pop; arg'
  | Ast.ByRef x ->
    ByRef x

and tc_expr fenv venv = pfunction
  | Ast.True ->
    z3_push Z.true_;
    (True, BaseET(mkpos Bool, mkpos Fixed Public))
  | Ast.False ->
    z3_push Z.false_;
    (False, BaseET(mkpos Bool, mkpos Fixed Public))
  | Ast.IntLiteral n ->
    z3_push @@ Z.num n;
    (IntLiteral n, BaseET(mkpos Num(abs n,n < 0), mkpos Fixed Public))
  | Ast.Variable x ->
    z3_push @@ Z.var x.data;
    let xref = Env.find_var venv x in
      (Variable x, refvt_to_etype' xref)
  | Ast.IntCast(b,e) ->
    let b' = bconv b in
      if not (is_int b') then raise @@ err(b'.pos);
    let e' = tc_expr fenv venv e in
      if not (is_int (expr_to_btype e')) then raise @@ err(e'.pos);
    let ml = expr_to_ml e' in
      z3_push @@ Z.intcast z3_pop (z3_ty (type_of e')) (z3_bty b');
      (IntCast(b',e'), BaseET(b',ml))
  | Ast.Declassify e ->
    let e' = tc_expr fenv venv e in
      (Declassify e', BaseET(expr_to_btype e', mkpos Fixed Public))
  | Ast.UnOp(op,e) ->
    let e' = tc_expr fenv venv e in
      z3_push @@ z3_unop op @@ z3_pop;
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
    let c = z3_pop in
    let b = z3_pop in
    let a = z3_pop in
      z3_push @@ Z.ite a b c;
      (TernOp(e1',e2',e3'), join_ty' p (type_of e2') (type_of e3'))
  | Ast.FnCall(f,args) ->
    let args' = List.map (tc_arg fenv venv) args in
    let (FunDec(_,Some rty,_,_)) = (Env.find_var fenv f).data in
      z3_push @@ Z.thing (z3_ty rty);
      (FnCall(f,args'), rty.data)

let rec tc_stm fenv venv = pfunction
  | Ast.BaseDec(x,vt,e) ->
    let e' = tc_expr fenv venv e in
    let ety = type_of e' in
    let vt' = refvt_conv vt in
    let xty = refvt_to_etype vt' in
      if not (ety <:$ xty) then raise @@ err(e'.pos);
      Env.add_var venv x vt';
      let zvar = Z.new_var (z3_ty xty) x.data in
        Z.(add (zvar = z3_pop));
      BaseDec(x,vt',e')
  | Ast.BaseAssign(x,e) ->
    let e' = tc_expr fenv venv e in
      z3_pop;
      BaseAssign(x,e')
  | Ast.If(cond,thenstms,elsestms) ->
    let cond' = tc_expr fenv venv cond in
    let thenstms' = tc_block fenv (Env.sub_env venv) thenstms in
    let elsestms' = tc_block fenv (Env.sub_env venv) elsestms in
      z3_pop;
    If(cond',thenstms',elsestms')
  | Ast.For(i,ity,lo,hi,stms) ->
    let ity' = bconv ity in
    let lo' = tc_expr fenv venv lo in
    let hi' = tc_expr fenv venv hi in
      z3_pop; z3_pop;
    let venv' = Env.sub_env venv in
      Env.add_var venv' i (mkpos RefVT(ity',mkpos Fixed Public,mkpos Const));
      let stms' = tc_block fenv venv' stms in
        For(i,ity',lo',hi',stms')
  | Ast.VoidFnCall(f,args) ->
    let args' = List.map (tc_arg fenv venv) args in
    let (FunDec(_,Some rty,_,_)) = (Env.find_var fenv f).data in
      VoidFnCall(f,args')
  | Ast.Return e ->
    let e' = tc_expr fenv venv e in
      z3_pop;
      Return e'
  | Ast.VoidReturn -> VoidReturn

and tc_block fenv venv stms =
  let stms' = List.map (tc_stm fenv venv) stms in
    (venv, stms')

let tc_param = pfunction
  | Ast.Param(x,vty) ->
    Param(x,refvt_conv vty)

let tc_fdec' fenv = function
  | Ast.FunDec(f,Some rt,params,stms) ->
    let rt' = etype_conv rt in
    let params' = List.map tc_param params in
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
    print_endline (Env.show_env pp_function_dec fenv);
    ret
