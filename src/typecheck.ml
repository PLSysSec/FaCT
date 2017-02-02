open Ast
open Env

exception NotImplemented
exception VariableNotDefined of string
exception FunctionNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string
exception InternalCompilerError of string

let errVarNotDefined v =
  VariableNotDefined("Variable `" ^ v ^ "` not defined")
let errFnNotDefined v =
  FunctionNotDefined("Function `" ^ v ^ "` not defined")
let errFoundNotVar v =
  TypeError("Cannot use `" ^ v ^ "` as variable")
let errFoundNotFn v =
  TypeError("Cannot use `" ^ v ^ "` as function")
let errTypeError =
  TypeError("Types cannot be unified for given operation")
let errFlowError =
  TypeError("Invalid type flow")
let errPassError =
  TypeError("Cannot call function with this type")

let isInt = function
  | NumericTop -> true
  | Int _ -> true
  | UInt _ -> true
  | NumericBottom -> true
  | _ -> false

let isBool = function
  | Bool -> true
  | _ -> false

let unify_ty t1 t2 =
  match (t1,t2) with
    | _ when t1 = t2 -> t1
    | (NumericTop, a) when isInt a -> NumericTop
    | (a, NumericTop) when isInt a -> NumericTop
    | (Int a, Int b) -> Int (max a b)
    | (UInt a, UInt b) -> UInt (max a b)
    | (NumericBottom, a) when isInt a -> a
    | (a, NumericBottom) when isInt a -> a
    | _ -> raise (TypeError(ty_to_string(t1) ^ " does not unify with " ^ ty_to_string(t2)))

let unify_label lbl1 lbl2 =
  match lbl1,lbl2 with
    | Some Secret,_ -> Some Secret
    | _,Some Secret -> Some Secret
    | Some Public,Some Public -> Some Public
    | _ -> None

let tc_prim = function
  | Number n -> NumericBottom, Some Public
  | ByteArray s -> Array { size=(List.length s); ty=NumericBottom }, Some Public
  | Boolean b -> Bool, Some Public

(* See note below *)
let tc_unop op ty =
  match ty with
    | (Some ty') ->
      (match op with
        | Neg when isInt ty' -> ty
        | L_Not when isBool ty' -> ty
        | B_Not when isInt ty' -> ty
        | _ -> raise errTypeError)
    | _ -> raise errTypeError

(* Note: apparently C just turns all bitwise operations (including shifts)
   into ints. Should we match C or do it our way?
   [scauligi] My vote is for keeping it our way. *)
let tc_binop op lhs' rhs' =
  match lhs',rhs' with
    | Some lhs,Some rhs ->
      (match op with
        | Plus when isInt(lhs) && isInt(rhs) -> unify_ty lhs rhs
        | Minus when isInt(lhs) && isInt(rhs) -> unify_ty lhs rhs
        | Multiply when isInt(lhs) && isInt(rhs) -> unify_ty lhs rhs
        | Equal when isInt(lhs) && isInt(rhs) -> Bool
        | NEqual when isInt(lhs) && isInt(rhs) -> Bool
        | GT when isInt(lhs) && isInt(rhs) -> Bool
        | GTE when isInt(lhs) && isInt(rhs) -> Bool
        | LT when isInt(lhs) && isInt(rhs) -> Bool
        | LTE when isInt(lhs) && isInt(rhs) -> Bool
        | L_And when isBool(lhs) && isBool(rhs) -> Bool
        | L_Or when isBool(lhs) && isBool(rhs) -> Bool
        | B_And when isInt(lhs) && isInt(rhs) -> unify_ty lhs rhs
        | B_Or when isInt(lhs) && isInt(rhs) -> unify_ty lhs rhs
        | B_Xor when isInt(lhs) && isInt(rhs) -> unify_ty lhs rhs
        | LeftShift when isInt(lhs) && isInt(rhs) -> lhs
        | RightShift when isInt(lhs) && isInt(rhs) -> lhs
        | _ -> raise errTypeError)
    | _ -> raise errTypeError

(* ctype -> ctype option -> unit *)
let ty_can_flow lhs rhs =
  match lhs, rhs with
    | _, None -> raise NotImplemented
    | a, Some b when a = b -> ()
    | Int a, Some (Int b) when a > b -> ()
    | Int _, Some NumericBottom -> ()
    | UInt a, Some (UInt b) when a > b -> ()
    | UInt _, Some NumericBottom -> ()
    | _ -> raise errPassError

let ty_can_pass lhs rhs =
  match lhs, rhs with
    | { kind=Out }, _ -> raise NotImplemented
    | _, { kind=Out } -> raise NotImplemented
    | { kind=Ref }, { kind=Val } -> raise errPassError
    | { ty=a }, { ty=b } when a = b -> ()
    | _ -> raise errPassError

let rec fill_public' venv expr =
  let fill v =
    match Hashtbl.find venv v with
      | VarEntry lt when !lt.label = None -> update_label venv v (Some Public)
      | _ -> () in
  let { e } = expr in
    match e with
      | VarExp v -> fill v
      | ArrExp(v,_) -> fill v
      | UnOp(_,e1) -> fill_public venv e1
      | BinOp(_,e1,e2) ->
        fill_public venv e1;
        fill_public venv e2
      | Primitive _ -> ()
      | CallExp(_,args) ->
        ignore(List.map (unpack (function
                          | ValArg e -> fill_public venv e
                          | VarArg(_,v) -> fill v
                          | ArrArg(_,v) -> fill v))
                 args)
and fill_public venv = unpack (fill_public' venv)

(* env -> labeled_type -> expr -> labeled_type *)
let lbl_can_flow venv lt e =
  ignore(lt.kind);
  match lt.label, e.data.label with
    | Some Public, Some Secret -> raise errFlowError
    | Some Public, None -> fill_public venv e; lt
    | None, Some Secret -> { lt with label=(Some Secret) }
    | _ -> lt

(* env -> labeled_type -> (arg * labeled_type) -> labeled_type *)
let lbl_can_pass venv lhs (arg,rhs) =
  let update_venv = function
    | VarArg(_,name)
    | ArrArg(_,name) -> update_label venv name (Some Public)
    | _ -> raise NotImplemented
  in
  match lhs, rhs with
    | { kind=Out }, _ -> raise NotImplemented
    | _, { kind=Out } -> raise NotImplemented
    | { kind=Ref }, { kind=Val } -> raise errPassError
    | { label=Some Public }, { label=Some Secret } -> raise errPassError
    | { kind=Ref; label=Some Secret }, { kind=Ref; label=Some Public } -> raise errPassError
    | { label=None }, { label=Some Secret } -> { lhs with label=Some Secret }
    | { label=Some Public }, { label=None } -> update_venv arg; lhs
    | { kind=Val; label=Some Secret }, _ -> lhs
    | _ -> lhs

let can_pass venv lhs (arg',rhs) =
  let { data=arg } = arg' in
  match arg with
  | ValArg e ->
    ignore(lhs.kind);
    ty_can_flow lhs.ty e.data.ty;
    ignore(lbl_can_flow venv lhs e)
  | VarArg(kind,name) ->
    let rhs' = (match rhs with Some lt -> lt | _ -> raise NotImplemented) in
      ty_can_pass lhs rhs';
      ignore(lbl_can_pass venv lhs (arg,rhs'))
  | ArrArg(kind,name) ->
    let rhs' = (match rhs with Some lt -> lt | _ -> raise NotImplemented) in
      ty_can_pass lhs rhs';
      ignore(lbl_can_pass venv lhs (arg,rhs'))

let rec tc_arg venv { pos; data } =
  let make_pos arg = { pos=pos; data=arg } in
  match data with
  | ValArg e ->
    let e' = tc_expr venv e in
      make_pos (ValArg e'),None
  | VarArg(kind,name) ->
    (try
       match Hashtbl.find venv name with
         | VarEntry lt -> make_pos (VarArg(!lt.kind,name)),Some !lt
         | _ -> raise NotImplemented
     with
       Not_found -> raise (errVarNotDefined name))
  | _ -> raise NotImplemented

(* fills ty and lbl strictly via unify *)
and tc_expr' venv expr =
  let make_exp e ty lbl = { e=e; ty=ty; label=lbl } in
  let { e; ty; label=lbl } = expr in
    match e with
      | VarExp v ->
        (try
           match Hashtbl.find venv v with
             | VarEntry lt ->
               make_exp e (Some !lt.ty) !lt.label
             | _ -> raise (errFoundNotVar v)
         with
             Not_found -> raise (errVarNotDefined v))
      | ArrExp(v,i) ->
        let i' = tc_expr venv i in
          (try
             match Hashtbl.find venv v with
               | VarEntry { contents=({ ty=(Array a) } as lt) } ->
                 make_exp (ArrExp(v,i')) (Some a.ty) lt.label
               | _ -> raise (errFoundNotVar v)
           with
               Not_found -> raise (errVarNotDefined v))
      | UnOp(op,e1) ->
        let e1' = tc_expr venv e1 in
        let ty' = tc_unop op e1'.data.ty in
          make_exp (UnOp(op,e1')) ty' lbl
      | BinOp(op,e1,e2) ->
        let e1' = tc_expr venv e1 in
        let e2' = tc_expr venv e2 in
        let ty' = tc_binop op e1'.data.ty e2'.data.ty in
        let lbl' = unify_label e1'.data.label e2'.data.label in
          make_exp (BinOp(op,e1',e2')) (Some ty') lbl'
      | Primitive prim ->
        let ty',lbl' = tc_prim prim in
          make_exp e (Some ty') lbl'
      | CallExp(name,args) ->
        let args' = List.map (tc_arg venv) args in
          (try
             match Hashtbl.find venv name with
               | FunEntry { f_ty=rty; f_args=params } ->
                 ignore(List.map2 (can_pass venv) params args');
                 let args'' = List.map fst args' in
                   make_exp (CallExp(name,args'')) (Some rty.ty) rty.label
               | _ -> raise (errFoundNotFn name)
           with
               Not_found -> raise (errFnNotDefined name))
and tc_expr venv = posmap (tc_expr' venv)

and can_flow venv lt expr =
  ignore(lt.kind);
  ty_can_flow lt.ty expr.data.ty;
  lbl_can_flow venv lt expr

and tc_stm venv fn_ty lbl_ctx { pos=p; data=stm } =
  let unify_ctx' e = { e with label=(unify_label lbl_ctx e.label) } in
  let unify_ctx = posmap unify_ctx' in
  match stm with
  | VarDec(name,lt,expr) ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let lt' = can_flow venv lt expr' in
      Hashtbl.add venv name (VarEntry (ref lt'));
      VarDec(name,lt',expr'), Some Public
  | Assign(name,expr) ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (errVarNotDefined name) in
      (match v with
        | VarEntry { contents={ kind=Val } } ->
          raise (TypeError("Cannot assign to read-only variable @ " ^ pos_string p))
        | VarEntry lt ->
          let lt' = can_flow venv !lt expr' in
            if !lt.label = None then update_label venv name lt'.label;
            Assign(name,expr'), Some Public
        | _ -> raise (errFoundNotVar name))
  | ArrAssign(name,index,expr) ->
    let index' = tc_expr venv index in
    let public_num = { ty=NumericTop; label=Some Public; kind=Val } in
    let _ = can_flow venv public_num index' in
    let expr' = unify_ctx (tc_expr venv expr) in
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (errVarNotDefined name) in
      (match v with
        | VarEntry { contents={ kind=Val } } ->
          raise (TypeError("Cannot assign to read-only array @ " ^ pos_string p))
        | VarEntry { contents=({ ty=(Array a); kind=_ } as lt) } ->
          let lte = { lt with ty=a.ty } in
          let lte' = can_flow venv lte expr' in
          let lt' = { lt with label=lte'.label } in
            if lt.label = None then update_label venv name lt'.label;
            ArrAssign(name,index',expr'), Some Public
        | _ -> raise (errFoundNotVar name))
  | If(cond,tstms,fstms) ->
    (* TODO: implement 2 if statements in the core language:
       a constant if and non constant if. *)
    let cond' = tc_expr venv cond in
    ignore(ty_can_flow Bool cond'.data.ty);
    let lbl_ctx' = unify_label lbl_ctx cond'.data.label in
    let tstms',ctx_t = tc_block (Hashtbl.copy venv) fn_ty lbl_ctx' tstms in
    let fstms',ctx_f = tc_block (Hashtbl.copy venv) fn_ty lbl_ctx' fstms in
      If(cond',tstms',fstms'), (unify_label ctx_t ctx_f)
  | For(name,ty,l,h,body) ->
    let l' = tc_expr venv l in
    let h' = tc_expr venv h in
    let lt = { ty=ty; label=Some Public; kind=Val } in
    ignore(can_flow venv lt l');
    ignore(can_flow venv lt h');
    let venv' = Hashtbl.copy venv in
      Hashtbl.add venv' name (VarEntry (ref { ty=ty; label=Some Public; kind=Val }));
      let body',_ = tc_block venv' fn_ty lbl_ctx body in
        For(name,ty,l',h',body'), Some Public
  | Return expr ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let fn_ty' = can_flow venv fn_ty expr' in
      Return expr', lbl_ctx

and tc_stms venv fn_ty lbl_ctx = function
  | [] -> [], lbl_ctx
  | stm::stms ->
    let stm',lbl_ctx' = tc_stm venv fn_ty lbl_ctx stm in
    let stm' = { stm with data=stm' } in
    let lbl_ctx' = unify_label lbl_ctx lbl_ctx' in
    let stms',_ = tc_stms venv fn_ty lbl_ctx' stms in
      stm'::stms', lbl_ctx'

and set_missing_labels_to_public venv stms =
  ignore(List.map (fun { data=s' } ->
                    match s' with
                      | VarDec(_,_,e) -> fill_public venv e
                      | Assign(_,e) -> fill_public venv e
                      | ArrAssign(_,i,e) -> fill_public venv i; fill_public venv e
                      | If(e,s1,s2) ->
                        fill_public venv e;
                        set_missing_labels_to_public venv s1;
                        set_missing_labels_to_public venv s2
                      | For(_,_,l,h,s) ->
                        fill_public venv l;
                        fill_public venv h;
                        set_missing_labels_to_public venv s
                      | Return e -> fill_public venv e)
           stms)

and tc_block venv fn_ty lbl_ctx stms =
  let stms',_ = tc_stms venv fn_ty lbl_ctx stms in
    set_missing_labels_to_public venv stms';
    tc_stms venv fn_ty lbl_ctx stms'

and tc_fdec venv { pos; data=fdec } =
  match fdec with
    | { rty={ ty=(Array _) } } ->
      raise (TypeError ("Function cannot return an array @ " ^ pos_string pos))
    | _ ->
      let venv' = Hashtbl.copy venv in
      let args_ty = List.map (fun { name=n; lt=lt } ->
                               Hashtbl.add venv' n (VarEntry (ref lt)); lt)
                      fdec.params in
      let rty = { ty=fdec.rty.ty; label=fdec.rty.label; kind=Val } in
      let body',_ = tc_block venv' rty (Some Public) fdec.body in
      let fdec' = { fdec with body=body' } in
        Hashtbl.add venv fdec.name (FunEntry { f_ty=fdec'.rty; f_args=args_ty });
        fdec'

and tc_module (CModule l) =
  ignore(List.map (tc_fdec Env.venv) l)
