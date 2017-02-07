open Pos
open Err
open Ast
open Env
open Tast

let is_int = function
  | Int _ -> true
  | UInt _ -> true
  | _ -> false

let is_bool = function
  | Bool -> true
  | _ -> false

let fit_num n =
  let numbits = Z.numbits n in
    if Z.sign n = -1 then Int (Z.of_int @@ numbits + 1)
    else UInt (Z.of_int numbits)

(* ctype -> ctype -> ctype *)
let unify_ty t1 t2 =
  match (t1,t2) with
    | _ when t1 = t2 -> t1
    | (Int a, Int b) -> Int Z.(max a b)
    | (UInt a, UInt b) -> UInt Z.(max a b)
    | (Int a, UInt b) -> Int Z.(max a (~$2 * b))
    | (UInt a, Int b) -> Int Z.(max (~$2 * a) b)
    | _ -> raise (TypeError(ty_to_string(t1) ^ " does not unify with " ^ ty_to_string(t2)))

(* label -> label -> label *)
let unify_label lbl1 lbl2 =
  match lbl1,lbl2 with
    | Secret,_ -> Secret
    | _,Secret -> Secret
    | Public,Public -> Public
    | _ -> Unknown

(* See note below *)
(* op -> ctype -> ctype *)
let tc_unop { pos=p; data=op } ty =
  match op with
    | Neg when is_int ty -> ty
    | L_Not when is_bool ty -> ty
    | B_Not when is_int ty -> ty
    | _ -> raise @@ errTypeError p

(* Note: apparently C just turns all bitwise operations (including shifts)
   into ints. Should we match C or do it our way?
   [scauligi] My vote is for keeping it our way. *)
(* op -> ctype -> ctype -> ctype *)
let tc_binop { pos=p; data=op } lhs rhs =
  match op with
    | Plus when is_int(lhs) && is_int(rhs) -> unify_ty lhs rhs
    | Minus when is_int(lhs) && is_int(rhs) -> unify_ty lhs rhs
    | Multiply when is_int(lhs) && is_int(rhs) -> unify_ty lhs rhs
    | Equal when is_int(lhs) && is_int(rhs) -> Bool
    | NEqual when is_int(lhs) && is_int(rhs) -> Bool
    | GT when is_int(lhs) && is_int(rhs) -> Bool
    | GTE when is_int(lhs) && is_int(rhs) -> Bool
    | LT when is_int(lhs) && is_int(rhs) -> Bool
    | LTE when is_int(lhs) && is_int(rhs) -> Bool
    | L_And when is_bool(lhs) && is_bool(rhs) -> Bool
    | L_Or when is_bool(lhs) && is_bool(rhs) -> Bool
    | B_And when is_int(lhs) && is_int(rhs) -> unify_ty lhs rhs
    | B_Or when is_int(lhs) && is_int(rhs) -> unify_ty lhs rhs
    | B_Xor when is_int(lhs) && is_int(rhs) -> unify_ty lhs rhs
    | LeftShift when is_int(lhs) && is_int(rhs) -> lhs
    | RightShift when is_int(lhs) && is_int(rhs) -> lhs
    | _ -> raise @@ errTypeError p

(* ctype -> ctype -> unit *)
let ty_can_flow p lhs rhs =
  match lhs, rhs with
    | a, b when a = b -> ()
    | Int a, Int b when a > b -> ()
    | UInt a, UInt b when a > b -> ()
    | Int a, UInt b when a > Z.(~$2 * b) -> ()
    | _ -> raise @@ errPassError p

(* labeled_type -> labeled_type -> unit *)
let ty_can_pass p lhs rhs =
  match lhs, rhs with
    | { kind=Out }, _ -> raise NotImplemented
    | _, { kind=Out } -> raise NotImplemented
    | { kind=Ref }, { kind=Val } -> raise @@ errPassError p
    | { ty=a }, { ty=b } when a = b -> ()
    | _ -> raise @@ errPassError p

(* name -> unit *)
let update_public venv p v =
  let lt = get_var venv v in
    if lt.label = Unknown then update_label venv v Public

(* targ -> unit *)
let rec fill_arg venv { pos=p; data=targ } =
  match targ with
    | TValArg texpr -> fill_public venv texpr
    | TVarArg(name,_)
    | TArrArg(name,_) -> update_public venv p name

(* texpr -> unit *)
and fill_public venv { pos=p; data=te } =
  match te.e with
    | TVarExp v -> update_public venv p v
    | TArrExp(v,_) -> update_public venv p v
    | TUnOp(_,e1) -> fill_public venv e1
    | TBinOp(_,e1,e2) ->
      fill_public venv e1;
      fill_public venv e2
    | TPrimitive _ -> ()
    | TCallExp(_,targs) ->
      ignore(List.map (fill_arg venv) targs)

(* labeled_type -> texpr -> labeled_type *)
let lbl_can_flow venv lt ({ pos=p; data=te } as pte) =
  match lt.label, te.e_lbl with
    | Public, Secret -> raise @@ errFlowError p
    | Public, Unknown -> fill_public venv pte; lt
    | Unknown, Secret -> { lt with label=Secret }
    | _ -> lt

(* labeled_type -> targ -> labeled_type *)
let lbl_can_pass venv lhs ({ pos=p; data=targ } as ptarg) =
  let rhs =
    match targ with
      | TValArg te -> { ty=te.data.e_ty; label=te.data.e_lbl; kind=Val }
      | TVarArg(name,lt) -> lt
      | _ -> raise NotImplemented
  in
  match lhs, rhs with
    | { kind=Out }, _ -> raise NotImplemented
    | _, { kind=Out } -> raise NotImplemented
    | { kind=Ref }, { kind=Val } -> raise @@ errPassError p
    | { label=Public }, { label=Secret } -> raise @@ errPassError p
    | { kind=Ref; label=Secret }, { kind=Ref; label=Public } -> raise @@ errPassError p
    | { label=Unknown }, { label=Secret } -> { lhs with label=Secret }
    | { label=Public }, { label=Unknown } -> fill_arg venv ptarg; lhs
    | { kind=Val; label=Secret }, _ -> lhs
    | _ -> lhs

(* labeled_type -> texpr -> labeled_type *)
let can_flow venv lt ({ pos=p; data=texpr } as ptexpr) =
  ty_can_flow p lt.ty texpr.e_ty;
  lbl_can_flow venv lt ptexpr

(* labeled_type -> targ -> labeled_type *)
let can_pass venv lhs ({ pos=p; data=targ } as ptarg) =
  ignore(
    match targ with
      | TValArg te ->
        ty_can_flow p lhs.ty te.data.e_ty;
      | TVarArg(name,kind) ->
        let rhs = get_var venv name in
          ty_can_pass p lhs rhs;
      | TArrArg(name,kind) ->
        let rhs = get_var venv name in
          ty_can_pass p lhs rhs);
  lbl_can_pass venv lhs ptarg

(* primitive -> texpr *)
let rec tc_prim venv { pos=p; data=expr } =
  let make_texpr prim ty lbl = { e=TPrimitive (make_ast p prim); e_ty=ty; e_lbl=lbl } in
  match expr with
    | Number n -> make_texpr (TNumber n) (fit_num n) Public
    | Boolean b -> make_texpr (TBoolean b) Bool Public
    | ArrayLiteral s ->
      let s' = List.map (tc_expr venv) s in
      let ty = List.fold_left
                 (fun a b -> unify_ty a b.data.e_ty)
                 ((List.hd s').data.e_ty) (List.tl s') in
      let lbl = List.fold_left
                  (fun a b -> unify_label a b.data.e_lbl)
                  ((List.hd s').data.e_lbl) (List.tl s') in
        make_texpr (TArrayLiteral s') (Array { size=Z.of_int (List.length s); ty=ty }) lbl

(* arg -> targ *)
and tc_arg venv { pos=p; data=arg } =
  let tc_arg' = function
    | ValArg e ->
      let e' = tc_expr venv e in
        TValArg e'
    | VarArg(kind,name) ->
      let lt = get_var venv name in
        (match kind,lt.kind with
          | Out,_ -> raise NotImplemented
          | _,Out -> raise NotImplemented
          | Ref,Val -> raise @@ err p
          | _ -> TVarArg(name, { lt with kind=kind }))
    | _ -> raise NotImplemented
  in make_ast p @@ tc_arg' arg

(* expr -> texpr *)
and tc_expr venv { pos=p; data=expr } =
  let tc_expr' = function
    | VarExp v ->
      let lt = get_var venv v in
        { e=TVarExp v; e_ty=lt.ty; e_lbl=lt.label }
    | ArrExp(v,i) ->
      let i' = tc_expr venv i in
      if not (is_int i'.data.e_ty) then raise @@ err p;
      (* TODO add dynamic bounds check *)
      let lta = get_var venv v in
        (match lta.ty with
          | Array { ty } -> { e=TArrExp(v,i'); e_ty=ty; e_lbl=lta.label }
          | _ -> raise @@ err p)
    | UnOp(op,e1) ->
      let e1' = tc_expr venv e1 in
      let ty' = tc_unop op e1'.data.e_ty in
        { e=TUnOp(op,e1'); e_ty=ty'; e_lbl=e1'.data.e_lbl }
    | BinOp(op,e1,e2) ->
      let e1' = tc_expr venv e1 in
      let e2' = tc_expr venv e2 in
      let ty' = tc_binop op e1'.data.e_ty e2'.data.e_ty in
      let lbl' = unify_label e1'.data.e_lbl e2'.data.e_lbl in
        { e=TBinOp(op,e1',e2'); e_ty=ty'; e_lbl=lbl' }
    | Primitive prim -> tc_prim venv prim
    | CallExp(name,args) ->
      let args' = List.map (tc_arg venv) args in
      let f = get_fn venv name in
        ignore(List.map2 (can_pass venv) f.f_args args'); (* TODO infer fn param labels *)
        { e=TCallExp(name,args'); e_ty=f.f_rty; e_lbl=f.f_rlbl }
  in make_ast p @@ tc_expr' expr

(* tstm list -> unit *)
let rec set_missing_labels_to_public venv stms =
  ignore(List.map (fun { data=s' } ->
                    match s' with
                      | TVarDec(_,_,e) -> fill_public venv e
                      | TAssign(_,e) -> fill_public venv e
                      | TArrAssign(_,i,e) -> fill_public venv i; fill_public venv e
                      | TIf(e,s1,s2) ->
                        fill_public venv e;
                        set_missing_labels_to_public s1.venv s1.body;
                        set_missing_labels_to_public s2.venv s2.body
                      | TFor(_,_,l,h,s) ->
                        fill_public venv l;
                        fill_public venv h;
                        set_missing_labels_to_public s.venv s.body
                      | TReturn e -> fill_public venv e)
           stms)

(* ret:lt -> ctx:lt -> stm -> (tstm * add_ctx:lt) *)
let rec tc_stm venv fn_ty lbl_ctx { pos=p; data=stm } =
  let unify_ctx' e = { e with e_lbl=(unify_label lbl_ctx e.e_lbl) } in
  let unify_ctx = posmap unify_ctx' in
  match stm with
  | VarDec(name,lt,expr) ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let lt' = can_flow venv lt expr' in
      Hashtbl.add venv name (VarEntry (ref lt'));
      TVarDec(name,lt',expr'), Public
  | Assign(name,expr) ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let lt = get_var venv name in
      if lt.kind = Val then raise (TypeError("Cannot assign to read-only variable @ " ^ pos_string p));
      let lt' = can_flow venv lt expr' in
        update_label venv name lt'.label;
        TAssign(name,expr'), Public
  | ArrAssign(name,i,expr) ->
    let i' = tc_expr venv i in
    if not (is_int i'.data.e_ty) then raise @@ err p;
    (* TODO add dynamic bounds check *)
    let expr' = unify_ctx (tc_expr venv expr) in
    let lt = get_var venv name in
      if lt.kind = Val then raise (TypeError("Cannot assign to read-only array @ " ^ pos_string p));
      (match lt.ty with
        | Array { ty } ->
          let lte = { lt with ty=ty } in
          let lte' = can_flow venv lte expr' in
          let lt' = { lt with label=lte'.label } in
            update_label venv name lt'.label;
            TArrAssign(name,i',expr'), Public
        | _ -> raise @@ err p)
  | If(cond,tstms,fstms) ->
    (* TODO: implement 2 if statements in the core language:
       a constant if and non constant if. *)
    let cond' = tc_expr venv cond in
    if not (is_bool cond'.data.e_ty) then raise @@ err p;
    let lbl_ctx' = unify_label lbl_ctx cond'.data.e_lbl in
    let tstms',ctx_t = tc_block (Hashtbl.copy venv) fn_ty lbl_ctx' tstms in
    let fstms',ctx_f = tc_block (Hashtbl.copy venv) fn_ty lbl_ctx' fstms in
      TIf(cond',tstms',fstms'), (unify_label ctx_t ctx_f)
  | For(name,ty,l,h,body) ->
    if not (is_int ty) then raise @@ err p;
    let l' = tc_expr venv l in
    let h' = tc_expr venv h in
    let lt = { ty=ty; label=Public; kind=Val } in
    ignore(can_flow venv lt l');
    ignore(can_flow venv lt h');
    let venv' = Hashtbl.copy venv in
      Hashtbl.add venv' name (VarEntry (ref { ty=ty; label=Public; kind=Val }));
      let body',_ = tc_block venv' fn_ty lbl_ctx body in
        TFor(name,ty,l',h',body'), Public
  | Return expr ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let fn_ty' = can_flow venv fn_ty expr' in
      TReturn expr', lbl_ctx

(* ret:lt -> ctx:lt -> stm list -> (tstm list * ctx:lt) *)
and tc_block venv fn_ty lbl_ctx stms =
  let stms',_ = tc_stms venv fn_ty lbl_ctx stms in
  set_missing_labels_to_public venv stms';
  (* XXX there should really be a better way of doing this
     but right now I'm purposely using stms and not stms'
     because stms is Ast and stms' is Tast and theoretically
     it should work out to the same, just with redundant work being done *)
  let stms',lbl = tc_stms venv fn_ty lbl_ctx stms in
    { venv=venv; body=stms' }, lbl

(* ret:lt -> ctx:lt -> stm list -> (tstm list * ctx:lt) *)
and tc_stms venv fn_ty lbl_ctx = function
  | [] -> [], lbl_ctx
  | stm::stms ->
    let stm',lbl_ctx' = tc_stm venv fn_ty lbl_ctx stm in
    let stm' = { pos=stm.pos; data=stm' } in
    let lbl_ctx' = unify_label lbl_ctx lbl_ctx' in
    let stms',_ = tc_stms venv fn_ty lbl_ctx' stms in
      stm'::stms', lbl_ctx'

let tc_fdec venv { pos=p; data=fdec } =
  match fdec with
    | { rty=Array _ } ->
      raise (TypeError ("Function cannot return an array @ " ^ pos_string p))
    | _ ->
      let venv' = Hashtbl.copy venv in
      ignore(List.map (fun { data={ name=n; lt=lt } } ->
                        Hashtbl.add venv' n (VarEntry (ref lt)))
               fdec.params);
      let rty = { ty=fdec.rty; label=fdec.rlbl; kind=Val } in
      let body',_ = tc_block venv' rty Public fdec.body in
      let fdec' = { t_name=fdec.name; t_params=fdec.params; t_rty=fdec.rty; t_rlbl=fdec.rlbl; t_body=body' } in
      let fdec' = { pos=p; data=fdec' } in
        update_fn venv fdec'; fdec'

let tc_module (CModule l) =
  TCModule (List.map (tc_fdec Env.venv) l)
