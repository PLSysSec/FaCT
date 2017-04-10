open Pos
open Err
open Ast
open Env
open Tast

let number = "Int or UInt"
let unsigned = "UInt"

let get_size p = function
  | Base(Int a) -> a
  | Base(UInt a) -> a
  | Base Bool -> 8
  | Arr _ -> raise_error p UnknownSize

let is_int = function
  | (Base (Int _)) -> true
  | (Base (UInt _)) -> true
  | _ -> false

let is_unsigned = function
  | (Base (UInt _)) -> true
  | _ -> false

let is_bool = function
  | (Base Bool) -> true
  | _ -> false

let fit_num n =
  let rec numbits = function
    | 0  -> 1
    | 1  -> 1
    | -1 -> 2
    | n  -> 1 + (numbits (n / 2))
  in
    if n < 0 then Int (numbits n)
    else UInt (numbits n)

let unify_base_ty pos t1 t2 =
  match t1,t2 with
    | (Int a, Int b) -> Int (max a b)
    | (UInt a, UInt b) -> UInt (max a b)
    | (Int a, UInt b) -> Int (max a (2 * b))
    | (UInt a, Int b) -> Int (max (2 * a) b)
    | (e, a) ->
      raise_error pos (TypeError { expected=(Base e); actual=(Base a) })

let unify_ty pos t1 t2 =
  match (t1,t2) with
    | Base b1, Base b2 -> Base(unify_base_ty pos b1 b2)
    | Base b, Arr(t,i) -> raise_error pos (TypeError { expected=t1; actual=t2 })
    | Arr(t,i), Base b -> raise_error pos (TypeError { expected=t1; actual=t2 })
    | Arr(t,i), Arr(t',i') -> raise_error pos NotImplemented

let unify_base_sz pos t1 t2 =
  match (t1,t2) with
    | (Int a, Int b) -> Int (max a b)
    | (UInt a, UInt b) -> UInt (max a b)
    | (Int a, UInt b) -> Int (max a b)
    | (UInt a, Int b) -> Int (max a b)
    | (e, a) ->
      raise_error pos (TypeError { expected=(Base e); actual=(Base a) })

let unify_sz pos t1 t2 =
  match (t1,t2) with
    | _ when equal_ctype t1 t2 -> t1
    | Base b1, Base b2 -> Base(unify_base_sz pos b1 b2)
    | Base b, Arr(t,i) -> raise_error pos (TypeError { expected=t1; actual=t2 })
    | Arr(t,i), Base b -> raise_error pos (TypeError { expected=t1; actual=t2 })
    | Arr(t,i), Arr(t',i') -> raise_error pos NotImplemented

let unify_label lbl1 lbl2 =
  match lbl1,lbl2 with
    | Secret,_ -> Secret
    | _,Secret -> Secret
    | Public,Public -> Public
    | _ -> Unknown

let tc_unop { pos=p; data=op } ty =
  match op with
    | Negation when is_int ty -> ty
    | LogicalNot when is_bool ty -> ty
    | BitwiseNot when is_int ty -> ty
    | Negation ->
      raise_error p (TypeError { expected=(Base(Int 32)); actual=ty })
    | LogicalNot ->
      raise_error p (TypeError { expected=(Base Bool); actual=ty })
    | BitwiseNot ->
      raise_error p (TypeError { expected=(Base(Int 32)); actual=ty })

let tc_binop { pos=p; data=op } (lhs:ctype) (rhs:ctype) =
  match op with
    | Plus when is_int(lhs) && is_int(rhs) -> unify_ty p lhs rhs
    | Minus when is_int(lhs) && is_int(rhs) -> unify_ty p lhs rhs
    | Multiply when is_int(lhs) && is_int(rhs) -> unify_ty p lhs rhs
    | Equal when is_int(lhs) && is_int(rhs) -> Base Bool
    | NEqual when is_int(lhs) && is_int(rhs) -> Base Bool
    | GT when is_int(lhs) && is_int(rhs) -> Base Bool
    | GTE when is_int(lhs) && is_int(rhs) -> Base Bool
    | LT when is_int(lhs) && is_int(rhs) -> Base Bool
    | LTE when is_int(lhs) && is_int(rhs) -> Base Bool
    | LogicalAnd when is_bool(lhs) && is_bool(rhs) -> Base Bool
    | LogicalOr when is_bool(lhs) && is_bool(rhs) -> Base Bool
    | BitwiseAnd when is_int(lhs) && is_int(rhs) -> unify_sz p lhs rhs
    | BitwiseOr when is_int(lhs) && is_int(rhs) -> unify_sz p lhs rhs
    | BitwiseXor when is_int(lhs) && is_int(rhs) -> unify_sz p lhs rhs
    | LeftShift when is_int(lhs) && is_unsigned(rhs) -> lhs
    | RightShift when is_int(lhs) && is_unsigned(rhs) -> lhs
    | Plus
    | Minus
    | Multiply
    | Equal
    | NEqual
    | GT
    | GTE
    | LT
    | LTE when is_bool(lhs) || is_bool(rhs) ->
      raise_error p (TypeErrorGeneric { expected=number; actual=(Base Bool) })
    | LogicalAnd
    | LogicalOr when is_int(lhs) ->
      raise_error p (TypeError { expected=(Base Bool); actual=lhs })
    | LogicalAnd
    | LogicalOr when is_int(rhs) ->
      raise_error p (TypeError { expected=(Base Bool); actual=rhs })
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor when is_bool(lhs) || is_bool(rhs) ->
      raise_error p (TypeErrorGeneric { expected=number; actual=(Base Bool) })
    | LeftShift when is_bool(lhs) ->
      raise_error p (TypeErrorGeneric { expected=number; actual=lhs })
    | LeftShift ->
      raise_error p (TypeErrorGeneric { expected=unsigned; actual=rhs })
    | RightShift when is_bool(lhs) ->
      raise_error p (TypeErrorGeneric { expected=number; actual=lhs })
    | RightShift ->
      raise_error p (TypeErrorGeneric { expected=unsigned; actual=rhs })
    | _ -> raise_compiler_bug p UnmatchedTypeError

let rec update_public venv p v =
  let des = get_var venv v p in
  match des.label with
    | Unknown -> update_label venv v { des with label=Public } p
    | Secret
    | Public -> ()

and fill_public venv { pos=p; data=te } =
  match te.expr with
    | TBoolean b -> ()
    | TNumber n -> ()
    | TVar v -> update_public venv p v
    | TArrAccess(n,e) ->
      update_public venv p n;
      fill_public venv e
    | TArrComprehension(bt,s,bt',n,e) ->
      raise_error p NotImplemented
    | TArrView(n,e,s) -> update_public venv p n
    | TUnOp(_,e1) -> fill_public venv e1
    | TBinOp(_,e1,e2) ->
      fill_public venv e1;
      fill_public venv e2
    | TTernaryOp(e,e1,e2) ->
      fill_public venv e;
      fill_public venv e1;
      fill_public venv e2;
    | TRef(s) ->
      update_public venv p s
    | TFunCall(_,targs) ->
      ignore(List.map (fill_public venv) targs)

let label_can_flow venv p lhs_lab rhs_lab rhs_expr =
  match lhs_lab, rhs_lab with
    | Secret, Secret
    | Secret, Unknown
    | Secret, Public
    | Unknown, Secret -> Secret
    | Public, Secret ->
      raise_error p (LabelFlowError { lhs=lhs_lab; rhs=rhs_lab })
    | Unknown, Public
    | Public, Public -> Public
    | Public, Unknown -> fill_public venv rhs_expr; Public
    | Unknown, Unknown -> Unknown

let mutability_can_flow p lhs rhs rhs_ty =
  match lhs, rhs, rhs_ty with
    | Const, Const, _ -> Const
    | Const, Mut, _ ->
      raise_error p (MutabilityFlowError { expected=Const; actual=Mut })
    | Mut, Mut, _ -> Mut
    | Mut, Const, Base(_) -> Mut
    | Mut, Const, Arr(_) ->
      raise_error p (MutabilityFlowError { expected=Mut; actual=Const })

let arr_assign_can_flow p lhs rhs =
  match lhs, rhs with
    | Arr(t,s), Base(b) when equal_base_type t b -> ()
    | Arr(UInt(a),s), Base(UInt b) when a >= b -> ()
    | Arr(Int(a),s), Base(UInt b) when a > (2 * b) -> ()
    | _ -> raise_error p (TypeFlowError { lhs; rhs })

let type_can_flow p lhs rhs =
  match lhs, rhs with
    | a, b when equal_ctype a b -> ()
    | Base(Int a), Base(Int b) when a > b -> ()
    | Base(UInt a), Base(UInt b) when a > b -> ()
    | Base(Int a), Base(UInt b) when a > (2 * b) -> ()
    | _ -> raise_error p (TypeFlowError { lhs; rhs })

let can_pass venv p lhs_des ({ data={ expr; description }} as rhs) =
  let { label; mutability; ctype } = description in
  ignore(mutability_can_flow p lhs_des.mutability mutability ctype);
  ignore(label_can_flow venv p lhs_des.label label rhs);
  type_can_flow p lhs_des.ctype ctype

let can_flow venv lhs ({ pos=p; data={ expr; description} } as ptexpr) =
  let { label; mutability; ctype } = description in
  type_can_flow p lhs.ctype ctype;
  let lbl = label_can_flow venv p lhs.label label ptexpr in
  let mut = mutability_can_flow p lhs.mutability mutability ctype in
  { lhs with label=lbl; mutability=mut }

let arr_assign_can_flow venv lhs ({ pos=p; data={ expr; description} } as ptexpr) =
  let { label; mutability; ctype } = description in
  arr_assign_can_flow p lhs.ctype ctype;
  let lbl = label_can_flow venv p lhs.label label ptexpr in
  let mut = mutability_can_flow p lhs.mutability mutability ctype in
  { lhs with label=lbl; mutability=mut }

let rec tc_expr fenv venv { pos=p; data=expr } : texpr =
  let tc_expr' = function
    | Var v ->
      let des = get_var venv v p in
      { expr=TVar v; description=des }
    | ArrAccess(v,i) ->
      let { pos; data } as t_i = tc_expr fenv venv i in
      ignore(label_can_flow venv p Public t_i.data.description.label t_i);
      let ty = data.description.ctype in
      (if not (is_unsigned ty)
        then let size = get_size p ty in
              raise_error p
                (TypeError { expected=(Base(UInt size)); actual=ty }));
      let des = get_arr venv v p in
      let Arr(base_ty,s) = des.ctype in
      { expr=TArrAccess(v,t_i); description={ des with ctype=Base(base_ty) }}
    | UnOp(op,e1) ->
      let t_e1 = tc_expr fenv venv e1 in
      let ty = tc_unop op t_e1.data.description.ctype in
      let des = t_e1.data.description in
      { expr=TUnOp(op,t_e1); description={ des with ctype=ty }}
    | BinOp(op,e1,e2) ->
      let t_e1 = tc_expr fenv venv e1 in
      let t_e2 = tc_expr fenv venv e2 in
      let t1 = t_e1.data.description.ctype in
      let t2 = t_e2.data.description.ctype in
      let ty = tc_binop op t1 t2 in
      let lab1 = t_e1.data.description.label in
      let lab2 = t_e2.data.description.label in
      let lab = unify_label lab1 lab2 in
      let des = { label=lab; mutability=Const; ctype=ty } in
      { expr=TBinOp(op,t_e1, t_e2); description=des }
    | Boolean b ->
      let des = { label=Public; mutability=Const; ctype=Base Bool } in
      { expr=TBoolean(b); description=des }
    | Number n ->
      let ty = Base(fit_num n) in
      let des = { label=Public; mutability=Const; ctype=ty } in
      { expr=TNumber(n); description=des }
    | FunCall(name,args) ->
      let args' = List.map (tc_expr fenv venv) args in
      let f = get_fn fenv name p in
      List.iter2 (can_pass venv p) f.f_args args';
      { expr=TFunCall(name,args'); description=f.f_rvt }
    | _ -> raise_error p UnmatchedTypeError
  in make_adt p @@ tc_expr' expr

let rec tc_stm fenv venv fn_des lbl_ctx { pos=p; data=stm } =
  let unify_ctx =
    let unify_ctx' e =
      let lab = unify_label lbl_ctx e.description.label in
      { e with description={ e.description with label=lab }} in
    posmap unify_ctx' in
  match stm with
  | VarDec(name,des,expr) ->
    let expr' = unify_ctx (tc_expr fenv venv expr) in
    let des' = can_flow venv des expr' in
    add_var venv name des' p;
    TVarDec(name,des',expr'), lbl_ctx
  | VarAssign(name,expr) ->
    let expr' = unify_ctx (tc_expr fenv venv expr) in
    let des = get_var venv name p in
    let des' = can_flow venv des expr' in
    update_label venv name des' p;
    TVarAssign(name,expr'), lbl_ctx
  | ArrAssign(name,i,expr) ->
    let i' = tc_expr fenv venv i in
    let ty = i'.data.description.ctype in
    (if not (is_unsigned ty)
      then let size = get_size p ty in
            raise_error p
            (TypeError { expected=(Base(UInt size)); actual=ty }));
    (* TODO add dynamic bounds check *)
    let expr' = unify_ctx (tc_expr fenv venv expr) in
    let des = get_arr venv name p in
    let des' = arr_assign_can_flow venv des expr' in
    update_label venv name des' p;
    TArrAssign(name,i',expr'), lbl_ctx
  
  | If(cond,tstms,fstms) ->
    (* TODO: I dont think this exactly follows the semantics.
              Check with Sunjay about this.. *)
    let cond' = tc_expr fenv venv cond in
    let cond_ty = cond'.data.description.ctype in
    if not (is_bool cond_ty)
      then raise_error p
            (TypeError { expected=Base(Bool); actual=cond_ty });
    let cond_lab = cond'.data.description.label in
    let lbl_ctx' = unify_label lbl_ctx cond_lab in
    let tstms',ctx_t = tc_stms fenv (Env.sub_env venv) fn_des lbl_ctx' tstms in
    let fstms',ctx_f = tc_stms fenv (Env.sub_env venv) fn_des lbl_ctx' fstms in
    TIf(cond',tstms',fstms'), (unify_label ctx_t ctx_f)
  | For(ty,name,l,h,body) ->
    let bt = Base ty in
    if not (is_int bt)
      then raise_error p (TypeErrorGeneric { expected=number; actual=bt });
    let l' = tc_expr fenv venv l in
    let h' = tc_expr fenv venv h in
    let des = { ctype=bt; label=Public; mutability=Const } in
    ignore(can_flow venv des l');
    ignore(can_flow venv des h');
    let venv' = Env.sub_env venv in
    add_var venv' name des p;
    let body',_ = tc_stms fenv venv' fn_des lbl_ctx body in
    TFor(ty,name,l',h',body'), lbl_ctx
  | Return expr ->
    let expr' = unify_ctx (tc_expr fenv venv expr) in
    ignore(can_flow venv fn_des expr');
    TReturn expr', lbl_ctx

and tc_stms fenv venv fn_des lbl_ctx = function
  | [] -> ([] : Tast.tstm list), lbl_ctx
  | stm::stms ->
    let stm',lbl_ctx' = tc_stm fenv venv fn_des lbl_ctx stm in
    let stm' : Tast.tstm = { pos=stm.pos; data=stm' } in
    let lbl_ctx' = unify_label lbl_ctx lbl_ctx' in
    let stms',_ = tc_stms fenv venv fn_des lbl_ctx' stms in
      stm'::stms', lbl_ctx'

and tc_param venv { pos=p; data=(name,des) } =
  add_var venv name des p;
  match des.label with
    | Unknown -> { pos=p; data=(name, { des with label=Public})}
    | _ -> { pos=p; data=(name,des)}

and tc_fdec fenv { pos=p; data=(fdec:Ast.fdec_base) } =
  let venv = new_env () in
  let params' = List.map (tc_param venv) fdec.params in
  let ret_des =
    match fdec.ret_description.label with
      | Unknown -> { fdec.ret_description with label=Secret }
      | _ -> fdec.ret_description in
  let body',_ = tc_stms fenv venv ret_des Public fdec.stms in
  Env.fill_vtbl_public venv;
  let fdec' = { name=fdec.name; params=params';
                ret_description=ret_des; tstms=body' } in
  let fdec' = { pos=p; data=fdec' } in
  Tast.update_fn fenv fdec';
  fdec'

and tc_module (Module fdecs) =
  let fenv = Env.new_fenv () in
  let t = List.map (tc_fdec fenv) fdecs in
  TCModule(fenv, t)