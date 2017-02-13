open Err
open Tast

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
  | Ast.Int n -> Cast.Int (Z.to_int n)
  | Ast.UInt n -> Cast.UInt (Z.to_int n)
  | Ast.Bool -> Cast.BoolMask

and transform_label = function
  | Ast.Public -> Cast.Public
  | Ast.Secret -> Cast.Secret
  | Ast.Unknown -> raise @@ TransformError "Encountered `Unknown` label"

and transform_kind = function
  | Ast.Val -> Cast.Val
  | Ast.Ref -> Cast.Ref
  | Ast.Arr s -> Cast.Arr (Z.to_int s)

and transform_vt { Ast.v_ty; Ast.v_lbl } =
  { Cast.v_ty=transform_type v_ty; Cast.v_lbl=transform_label v_lbl }

and transform_lt { Ast.ty=t; Ast.label=l; Ast.kind=k } =
    { Cast.ty=(transform_type t);
      Cast.lbl=(transform_label l);
      Cast.kind=(transform_kind k) }

and transform_param { Pos.data={Ast.name=n; Ast.lt=t} } =
    {Cast.name=n; Cast.lt=transform_lt(t)}

and transform_init { Pos.data=init } =
  match init with
    | Ast.ZeroArray -> Cast.ZeroArray

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
  | _ -> raise (UnclassifiedError("can't transform non-topvenv"))

and transform_subvenv venv = function
  | Env.SubEnv(vtbl,_) -> Env.SubEnv (transform_vtbl vtbl, venv)
  | _ -> raise (UnclassifiedError("can't transform non-topvenv"))

and venv_merge_up = function
  | Env.SubEnv(vtbl,venv) -> (* XXX need to check collisions *)
    let vtbl' = Env.get_vtbl venv in
      Hashtbl.iter (fun k v ->
                     Hashtbl.replace vtbl' k v)
        vtbl
  | _ -> raise (UnclassifiedError("can't merge up topvenv"))

and transform_stm' rty venv mem ctx stm =
  let make_expr e ty = { Cast.e=e; Cast.e_ty=ty } in

  let transform_block ctx b =
    let venv' = transform_subvenv venv b.venv in
    let mem' = Env.sub_env mem in
    let stms' = List.flatten(List.map (transform_stm rty venv' mem' ctx) b.body) in
      { Cast.venv=venv'; Cast.mem=mem'; Cast.body=stms' } in

  let get_var n =
    let lt = Env.get_var venv n in
      make_expr (Cast.VarExp n) lt.Cast.ty in

  let get_arr a i =
    let lt = Env.get_var venv a in
      match lt.Cast.kind with
        | Cast.Arr _ -> make_expr (Cast.ArrExp(a,i)) lt.Cast.ty
        | _ -> raise @@ errFoundNotArr a in


  let rset = make_expr (Cast.VarExp "__rset") Cast.BoolMask in
  let b_and l r = make_expr (Cast.BinOp(Cast.BitAnd,l,r)) (unify_ty l r) in
  let b_or l r = make_expr (Cast.BinOp(Cast.BitOr,l,r)) (unify_ty l r) in
  let b_not e = make_expr (Cast.UnOp(Cast.BitNot,e)) e.Cast.e_ty in

  match stm with
  | Tast.TVarDec(v,vt,e) ->
    let vt' = transform_vt(vt) in
    let e' = transform_expr(e) in
    [Cast.VarDec(v,vt',e')]
  | Tast.TArrDec(v,vt,s,init) ->
    let vt' = transform_vt(vt) in
    let s' = Z.to_int s in
    let init' = transform_init(init) in
    [Cast.ArrDec(v,vt',s',init')]
  | Tast.TAssign(v,e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let assign_ok = b_and c (b_not rset) in
    let newval = b_and e' assign_ok in
    let oldval = b_and (get_var v) (b_not assign_ok) in
    [Cast.Assign(v,(b_or newval oldval))]
  | Tast.TArrAssign(v,i,e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let assign_ok = b_and c (b_not rset) in
    let newval = b_and e' assign_ok in
    let i' = transform_expr i in
    let oldval = b_and (get_arr v i') (b_not assign_ok) in
    [Cast.ArrAssign(v,i',(b_or newval oldval))]
  | Tast.TIf(e,bt,bf) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let tname = new_temp_var() in
    let m = make_expr (Cast.VarExp tname) Cast.BoolMask in
    let c' = b_and m c in
    let ctx' = Context(c') in
    let vt = { Cast.v_ty=Cast.BoolMask; Cast.v_lbl=Cast.Secret } in
    let mdec = Cast.VarDec(tname,vt,b_and e' c) in
      Env.add_var venv tname (Cast.ltk vt Cast.Val);
    let bt' = transform_block ctx' bt in
    let bf' = transform_block ctx' bf in
      venv_merge_up bt'.Cast.venv;
      venv_merge_up bf'.Cast.venv;
    let mnot = Cast.Assign(tname,b_not m) in
    [mdec] @ bt'.Cast.body @ [mnot] @ bf'.Cast.body
  | Tast.TFor(n,t,l,h,b) ->
    let t' = transform_type t in
    let l' = transform_expr l in
    let h' = transform_expr h in
    let b' = transform_block ctx b in
    [Cast.For(n,t',l',h',b')]
  | Tast.TReturn(e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let rval = make_expr (Cast.VarExp "__rval") rty in
    let assign_ok = b_and c (b_not rset) in
    let newval = b_and e' assign_ok in
    [Cast.Assign("__rval",(b_or rval newval)); Cast.Assign("__rset",(b_or rset c))]
and transform_stm rty venv mem ctx = Pos.unpack (transform_stm' rty venv mem ctx)

and transform_arg = fun { Pos.data } ->
  let transform_arg' = function
    | Tast.TValArg e -> Cast.ValArg (transform_expr e)
    | Tast.TRefArg(n,vt) -> Cast.RefArg(n,transform_vt vt)
    | Tast.TArrArg(n,vt,sz) -> Cast.ArrArg(n,transform_vt vt,Z.to_int sz)
  in
    transform_arg' data

and transform_expr = fun { Pos.data } ->
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
  in { Cast.e=base_e; Cast.e_ty=ty }

and transform_primitive =
  let transform_primitive' = function
    | Tast.TNumber n -> Cast.Number (Z.to_int n) (* XXX *)
    | Tast.TBoolean true -> Cast.Mask (Cast.TRUE)
    | Tast.TBoolean false -> Cast.Mask (Cast.FALSE)
  in
    Pos.unpack transform_primitive'

and transform_unop =
  let transform_unop' = function
    | Ast.Neg -> Cast.Neg
    | Ast.L_Not -> Cast.BitNot
    | Ast.B_Not -> Cast.BitNot
  in
    Pos.unpack transform_unop'

and transform_binop =
  let transform_binop' = function
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
    | Ast.B_Xor -> Cast.BitXor
  in
    Pos.unpack transform_binop'

and transform_fdec { Pos.data } =
  let transform_fdec' = function
    | { Tast.t_name=name; Tast.t_params=args; Tast.t_rvt; Tast.t_body=body } ->
      let args' = List.map transform_param args in
      let rvt' = transform_vt(t_rvt) in
      let bm_false = { Cast.v_ty=Cast.BoolMask; Cast.v_lbl=Cast.Secret } in
      let prim_false = Cast.Primitive(Cast.Mask(Cast.FALSE)) in
      let bm_prim_false = { Cast.e=prim_false; Cast.e_ty=Cast.BoolMask } in
      let ctx = Context {
                  Cast.e=Cast.Primitive(Cast.Mask Cast.TRUE);
                  Cast.e_ty=Cast.BoolMask
                } in
      let r0 = { Cast.e=Cast.Primitive(Cast.Number 0); Cast.e_ty=rvt'.Cast.v_ty } in
      let rval = Cast.VarDec("__rval",rvt',r0) in
      let rset = Cast.VarDec("__rset",bm_false,bm_prim_false) in
      let venv' = transform_topvenv body.venv in
        Env.add_var venv' "__rval" (Cast.ltk rvt' Cast.Val);
        Env.add_var venv' "__rset" (Cast.ltk bm_false Cast.Val);
      let mem' = Env.new_env() in
      let body' = List.flatten(List.map (transform_stm rvt'.Cast.v_ty venv' mem' ctx) body.body) in
      let body'' = { Cast.venv=venv'; Cast.mem=mem'; Cast.body=[rval]@[rset]@body' } in
        Cast.FunctionDec(name,args',rvt',body'',
                         { Cast.e=Cast.VarExp("__rval");
                           Cast.e_ty=rvt'.Cast.v_ty })
  in
    transform_fdec' data
