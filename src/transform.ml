open Err
open Tast
open Pos

type context = Context of Cast.expr

let ctx_expr (Context(e)) = e

let new_temp_var =
  let cntr = ref 0 in
  let new_temp_var'() =
    cntr := !cntr + 1;
    "__m" ^ (string_of_int !cntr)
  in
  new_temp_var'

let unify_ty e1 e2 =
  let t1,t2 = e1.Cast.e_ty,e2.Cast.e_ty in
    match t1,t2 with
      | _ when Cast.equal_ctype t1 t2 -> t1
      | Cast.Int a, Cast.Int b -> Cast.Int (max a b)
      | Cast.UInt a, Cast.UInt b -> Cast.UInt (max a b)
      | Cast.Int a, Cast.UInt b -> Cast.Int (max a (2*b))
      | Cast.UInt a, Cast.Int b -> Cast.Int (max (2*a) b)
      | _, Cast.BoolMask -> t1
      | Cast.BoolMask, _ -> t2

let unify_sz e1 e2 =
  let t1,t2 = e1.Cast.e_ty,e2.Cast.e_ty in
    match t1,t2 with
      | _ when Cast.equal_ctype t1 t2 -> t1
      | Cast.Int a, Cast.Int b -> Cast.Int (max a b)
      | Cast.UInt a, Cast.UInt b -> Cast.UInt (max a b)
      | Cast.Int a, Cast.UInt b -> Cast.Int (max a b)
      | Cast.UInt a, Cast.Int b -> Cast.Int (max a b)
      | _, Cast.BoolMask -> t1
      | Cast.BoolMask, _ -> t2

let rec transform = function
  | Tast.TCModule(fenv,fdecs) ->
    let fenv' = transform_fenv fenv in
    let fdecs' = List.map transform_fdec fdecs in
      Cast.CModule(fenv',fdecs')

and transform_type = function
  | Ast.Int n -> Cast.Int n
  | Ast.UInt n -> Cast.UInt n
  | Ast.Bool -> Cast.BoolMask

and transform_label = function
  | Ast.Public -> Cast.Public
  | Ast.Secret -> Cast.Secret
  | Ast.Unknown -> raise_error_np UnknownLabelError

and transform_kind = function
  | Ast.Val -> Cast.Val
  | Ast.Ref -> Cast.Ref
  | Ast.Arr s -> Cast.Arr s

and transform_vt { Ast.v_ty; Ast.v_lbl } =
  { Cast.v_ty=transform_type v_ty; Cast.v_lbl=transform_label v_lbl }

and transform_lt { Ast.ty=t; Ast.label=l; Ast.kind=k } =
    { Cast.ty=(transform_type t);
      Cast.lbl=(transform_label l);
      Cast.kind=(transform_kind k) }

and transform_param { data={Ast.name=n; Ast.lt=t}; pos } =
    make_adt pos {Cast.name=n; Cast.lt=transform_lt(t)}

and transform_init = function
  | { Pos.data=Ast.UnsafeNoInit; pos } ->
    { Pos.data=Cast.UnsafeNoInit; pos=pos }

and transform_fenv fenv =
  let fenv' = Env.new_fenv() in
    Hashtbl.iter
      (fun name { Env.f_rvt; Env.f_args } ->
         let fentry = { Cast.f_rvt=transform_vt f_rvt;
                        Cast.f_args=List.map transform_lt f_args }
         in Env.add_fn fenv' name fentry)
      fenv;
    fenv'

and transform_vtbl vtbl =
  let vtbl' = Hashtbl.create 10 in
    Hashtbl.iter
      (fun name lt ->
         let lt' = transform_lt !lt
         in Hashtbl.add vtbl' name (ref lt'))
      vtbl;
    vtbl'

and transform_topvenv = function
  | Env.TopEnv vtbl -> Env.TopEnv (transform_vtbl vtbl)
  | _ -> raise_error_np TransformError

and transform_subvenv venv = function
  | Env.SubEnv(vtbl,_) -> Env.SubEnv (transform_vtbl vtbl, venv)
  | _ -> raise_error_np TransformError

and venv_merge_up = function
  | Env.SubEnv(vtbl,venv) -> (* XXX need to check collisions *)
    let vtbl' = Env.get_vtbl venv in
      Hashtbl.iter (fun k v ->
                     Hashtbl.replace vtbl' k v)
        vtbl
  | _ -> raise_error_np TransformError

and transform_stm rty venv mem ctx { data; pos } =
  let make_expr e ty pos =
    let adt = make_adt pos e in
     { Cast.e=adt; Cast.e_ty=ty } in

  let transform_block ctx b =
    let venv' = transform_subvenv venv b.venv in
    let mem' = Env.sub_env mem in
    let stms' = List.flatten(List.map (transform_stm rty venv' mem' ctx) b.body) in
      { Cast.venv=venv'; Cast.mem=mem'; Cast.body=stms' } in

  let get_var n pos =
    let lt = Env.get_var venv n pos in
      make_expr (Cast.VarExp n) lt.Cast.ty pos in

  let get_arr a i pos =
    let lt = Env.get_var venv a pos in
      match lt.Cast.kind with
        | Cast.Arr _ -> make_expr (Cast.ArrExp(a,i)) lt.Cast.ty pos
        | _ -> raise_error pos (ArrayNotDefined a) in

  let rset pos = make_expr (Cast.VarExp "__rset") Cast.BoolMask pos in
  let b_and l r pos =
    let op = make_adt pos Cast.BitAnd in
    make_expr (Cast.BinOp(op,l,r)) (unify_ty l.data r.data) pos in
  let b_or l r pos =
    let op = make_adt pos Cast.BitOr in
    make_expr (Cast.BinOp(op,l,r)) (unify_ty l.data r.data) pos in
  let b_not e pos =
    let op = make_adt pos Cast.BitNot in
    make_expr (Cast.UnOp(op,e)) e.data.Cast.e_ty pos in

  match data with
  | Tast.TVarDec(v,vt,e) ->
    let vt' = transform_vt(vt) in
    let e' = transform_expr(e) in
    let stm = make_adt pos (Cast.VarDec(v,vt',e')) in
    [stm]
  | Tast.TArrDec(v,vt,s,init) ->
    let vt' = transform_vt(vt) in
    let init' = transform_init(init) in
    let stm = make_adt pos (Cast.ArrDec(v,vt',s,init')) in
    [stm]
  | Tast.TAssign(v,e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let rset' = make_adt pos (rset pos) in
    let b_not' = make_adt pos (b_not rset' pos) in
    let assign_ok = make_adt pos(b_and c b_not' pos) in
    let newval = make_adt pos (b_and e' assign_ok pos) in
    let not_assign_ok = make_adt pos (b_not assign_ok pos) in
    let var = make_adt pos (get_var v pos) in
    let oldval = make_adt pos (b_and var not_assign_ok pos) in
    let or_val = make_adt pos (b_or newval oldval pos) in
    let stm = make_adt pos (Cast.Assign(v,or_val)) in
    [stm]
  | Tast.TArrAssign(v,i,e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let rset' = make_adt pos (rset pos) in
    let not_rset = make_adt pos (b_not rset' pos) in
    let assign_ok = make_adt pos (b_and c not_rset pos) in
    let newval = make_adt pos (b_and e' assign_ok pos) in
    let i' = transform_expr i in
    let arr = make_adt pos (get_arr v i' pos) in
    let not_assign_ok = make_adt pos (b_not assign_ok pos) in
    let oldval = make_adt pos (b_and arr not_assign_ok pos) in
    let rhs_or = make_adt pos (b_or newval oldval pos) in
    let stm = make_adt pos (Cast.ArrAssign(v,i',rhs_or)) in
    [stm]
  | Tast.TIf(e,bt,bf) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let tname = new_temp_var() in
    let m = make_adt pos (make_expr (Cast.VarExp tname) Cast.BoolMask pos) in
    let c' = make_adt pos (b_and m c pos) in
    let ctx' = Context(c') in
    let vt = { Cast.v_ty=Cast.BoolMask; Cast.v_lbl=Cast.Secret } in
    let mdec_rhs = make_adt pos (b_and e' c pos) in
    let mdec = make_adt pos (Cast.VarDec(tname,vt,mdec_rhs)) in
    Env.add_var venv tname (Cast.ltk vt Cast.Val) pos;
    let bt' = transform_block ctx' bt in
    let bf' = transform_block ctx' bf in
    venv_merge_up bt'.Cast.venv;
    venv_merge_up bf'.Cast.venv;
    let mnot_rhs = make_adt pos (b_not m pos) in
    let mnot = make_adt pos (Cast.Assign(tname,mnot_rhs)) in
    [mdec] @ bt'.Cast.body @ [mnot] @ bf'.Cast.body
  | Tast.TFor(n,t,l,h,b) ->
    let t' = transform_type t in
    let l' = transform_expr l in
    let h' = transform_expr h in
    let b' = transform_block ctx b in
    let stm = make_adt pos (Cast.For(n,t',l',h',b')) in
    [stm]
  | Tast.TReturn(e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let rval = make_expr (Cast.VarExp "__rval") rty pos in
    let rval' = make_adt pos rval in
    let rset' = make_adt pos (rset pos) in
    let not_rset = make_adt pos (b_not rset' pos) in
    let assign_ok = make_adt pos (b_and c not_rset pos) in
    let newval = make_adt pos (b_and e' assign_ok pos) in
    let rval_rhs = make_adt pos (b_or rval' newval pos) in
    let rset_rhs = make_adt pos (b_or rset' c pos) in
    let rval_stm = make_adt pos (Cast.Assign("__rval",rval_rhs)) in
    let rset_stm = make_adt pos (Cast.Assign("__rset",rset_rhs)) in
    [rval_stm; rset_stm]

and transform_arg = fun { data; pos } ->
  let transform_arg' = function
    | Tast.TValArg e -> make_adt pos (Cast.ValArg (transform_expr e))
    | Tast.TRefArg(n,vt) -> make_adt pos (Cast.RefArg(n,transform_vt vt))
    | Tast.TArrArg(n,vt,sz) -> make_adt pos (Cast.ArrArg(n,transform_vt vt,sz))
  in
    transform_arg' data

and transform_expr = fun { data; pos } ->
  let { Tast.e; Tast.e_ty } = data in
  let ty = transform_type e_ty in
  let base_e =
    match e with
      | Tast.TVarExp(s) -> Cast.VarExp s
      | Tast.TArrExp(s,i) -> Cast.ArrExp(s,transform_expr i)
      | Tast.TUnOp(u,e) -> Cast.UnOp(transform_unop(u),transform_expr(e))
      | Tast.TBinOp(b,e1,e2) ->
        let b' = transform_binop b in
        let e1' = transform_expr e1 in
        let e2' = transform_expr e2 in
        Cast.BinOp(b',e1',e2')
      | Tast.TPrimitive(p) -> Cast.Primitive(transform_primitive p)
      | Tast.TCallExp(n,args) ->
        let args' = List.map transform_arg args in
        Cast.CallExp(n,args')
  in
  let base_e' = make_adt pos base_e in
  let base = { Cast.e=base_e'; Cast.e_ty=ty } in
  make_adt pos base

and transform_primitive { data; pos} =
  match data with
    | Tast.TNumber n ->
      let prim = make_adt pos (Cast.Number n) in
      make_adt pos prim.data
    | Tast.TBoolean true -> make_adt pos (Cast.Mask (make_adt pos (Cast.TRUE)))
    | Tast.TBoolean false ->
      make_adt pos (Cast.Mask (make_adt pos (Cast.FALSE)))

and transform_unop { data; pos } =
  let op = match data with
    | Ast.Neg -> Cast.Neg
    | Ast.L_Not -> Cast.BitNot
    | Ast.B_Not -> Cast.BitNot in
  make_adt pos op

and transform_binop { data; pos } =
  let op = match data with
    | Ast.Plus -> Cast.Plus
    | Ast.Minus -> Cast.Minus
    | Ast.Multiply -> Cast.Mult
    | Ast.GT -> Cast.GT
    | Ast.GTE -> Cast.GTE
    | Ast.LT -> Cast.LT
    | Ast.LTE -> Cast.LTE
    | Ast.Equal -> Cast.Eq
    | Ast.NEqual -> Cast.Neq
    | Ast.L_And -> Cast.BitAnd
    | Ast.L_Or -> Cast.BitOr
    | Ast.LeftShift -> Cast.LeftShift
    | Ast.RightShift -> Cast.RightShift
    | Ast.B_And -> Cast.BitAnd
    | Ast.B_Or -> Cast.BitOr
    | Ast.B_Xor -> Cast.BitXor in
  make_adt pos op

and transform_fdec { data; pos } =
  match data with
    | { Tast.t_name=name; Tast.t_params=args; Tast.t_rvt; Tast.t_body=body } ->
      let args' = List.map transform_param args in
      let rvt' = transform_vt(t_rvt) in
      let bm_false = { Cast.v_ty=Cast.BoolMask; Cast.v_lbl=Cast.Secret } in
      let false_mask = make_adt pos (Cast.Mask (make_adt pos (Cast.FALSE))) in
      let true_mask = make_adt pos (Cast.Mask (make_adt pos (Cast.TRUE))) in
      let prim_false = make_adt pos (Cast.Primitive(false_mask)) in
      let prim_true = make_adt pos (Cast.Primitive(true_mask)) in
      let bm_prim_false =
        make_adt pos { Cast.e=prim_false; Cast.e_ty=Cast.BoolMask } in
      let ctx_expr =
        make_adt pos { Cast.e=prim_true; Cast.e_ty=Cast.BoolMask } in
      let ctx = Context ctx_expr in
      let r0_zero = make_adt pos (Cast.Number 0) in
      let r0_prim = make_adt pos (Cast.Primitive r0_zero) in
      let r0 = make_adt pos { Cast.e=r0_prim; Cast.e_ty=rvt'.Cast.v_ty } in
      let rval = make_adt pos (Cast.VarDec("__rval",rvt',r0)) in
      let rset = make_adt pos (Cast.VarDec("__rset",bm_false,bm_prim_false)) in
      let venv' = transform_topvenv body.venv in
      Env.add_var venv' "__rval" (Cast.ltk rvt' Cast.Val) pos;
      Env.add_var venv' "__rset" (Cast.ltk bm_false Cast.Val) pos;
      let mem' = Env.new_env() in
      let body' = List.flatten(List.map (transform_stm rvt'.Cast.v_ty venv' mem' ctx) body.body) in
      let body'' = { Cast.venv=venv'; Cast.mem=mem'; Cast.body=[rval]@[rset]@body' } in
      let rval_var = make_adt pos (Cast.VarExp("__rval")) in
      let rval_expr =
        make_adt pos { Cast.e=rval_var; Cast.e_ty=rvt'.Cast.v_ty } in
      make_adt pos (Cast.FunctionDec(name,args',rvt',body'', rval_expr))