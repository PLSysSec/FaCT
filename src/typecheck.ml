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

(* Unifies ctype for expressions *)
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

let isInt = function
  | NumericTop -> true
  | Int _ -> true
  | UInt _ -> true
  | NumericBottom -> true
  | _ -> false

let isBool = function
  | Bool -> true
  | _ -> false

(* See note below *)
let tc_unop p ty = function
  | Neg when isInt ty -> ty
  | L_Not when isBool ty -> ty
  | B_Not when isInt ty -> ty
  | _ -> raise (errTypeError p)

(* Note: apparently C just turns all bitwise operations (including shifts)
   into ints. Should we match C or do it our way?
   [scauligi] My vote is for keeping it our way. *)
let tc_binop p lhs rhs = function
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
  | _ -> raise (errTypeError p)

and ty_can_pass lhs rhs =
  match lhs, rhs with
    | { kind=Out }, _ => raise NotImplemented
    | _, { kind=Out } => raise NotImplemented
    | { kind=Ref }, { kind=Val } -> raise errPassError
    | { ty=a }, { ty=b } when a = b -> ()
    | _ -> raise errPassError

and lbl_can_pass venv lhs (arg,rhs) =
  let update_venv = function
    | VarArg(_,name)
    | ArrArg(_,name) -> update_label venv name (Some Public)
  in
  match lhs, rhs with
    | { kind=Out }, _ => raise NotImplemented
    | _, { kind=Out } => raise NotImplemented
    | { kind=Ref }, { kind=Val } -> raise errPassError
    | { label=Some Public }, { label=Some Secret } -> raise errPassError
    | { kind=Ref; label=Some Secret }, { kind=Ref; label=Some Public } -> raise errPassError
    | { label=None }, { label=Secret } -> { lhs with label=Some Secret }
    | { label=Some Public }, { label=None } -> update_venv arg; lhs
    | { kind=Val; label=Some Secret }, _ -> lhs

and can_pass venv lhs (arg,rhs) =
  match arg with
  | ValArg e ->
    ty_can_flow lhs.ty e.ty;
    ignore(lbl_can_flow venv lhs e)
  | VarArg(kind,name) ->
    ty_can_pass lhs rhs;
    lbl_can_pass venv lhs (arg,rhs)
  | ArrArg(kind,name) ->
    ty_can_pass lhs rhs;
    lbl_can_pass venv lhs (arg,rhs)

let rec tc_arg' venv = function
  | ValArg e ->
    let e' = tc_expr e in
      ValArg e',{ ty=e'.ty; label=e'.label; kind=Val }
  | VarArg(kind,name) ->
    (try
       match Hashtbl.find venv name with
         | VarEntry lt -> VarEntry(lt.kind,name),lt
     with
       Not_found -> raise (errVarNotDefined name p))
and tc_arg = posmap tc_arg'

(* fills ty and lbl strictly via unify *)
and tc_expr' venv expr =
  let { e; ty; label=lbl } = expr in
  let ty',lbl' =
    (match e with
      | VarExp v ->
        (try
           match Hashtbl.find venv v with
             | VarEntry lt -> lt.ty,lt.label
             | _ -> raise (errFoundNotVar v)
         with
             Not_found -> raise (errVarNotDefined v))
      | ArrExp(v,i) ->
        let i' = tc_expr venv i in
          (try
             match Hashtbl.find venv v with
               | VarEntry { ty=(Array a) } -> a.ty,lt.label
               | _ -> raise (errFoundNotVar v)
           with
               Not_found -> raise (errVarNotDefined v))
      | UnOp(op,e1) ->
        let e1' = tc_expr venv e1 in
        let ty' = tc_unop e1'.ty in
          ty',lbl
      | BinOp(op,e1,e2) ->
        let e1' = tc_expr venv e1 in
        let e2' = tc_expr venv e2 in
        let ty' = tc_binop e1'.ty e2'.ty in
        let lbl' = unify_label e1'.label e2'.label in
          ty',lbl'
      | Primitive prim -> tc_prim prim
      | CallExp(name,args) ->
        let args' = List.map (tc_arg venv) args in
        (try
           match Hashtbl.find venv name with
             | FunEntry { f_ty=rty; f_args=params } ->
               ignore(List.map2 (can_pass venv) params args');
               rty.ty,rty.label
             | _ -> raise (errFoundNotFn name)
         with
             Not_found -> raise (errFnNotDefined name)))
  in
    { expr with ty=ty'; label=lbl' }
and tc_expr = posmap tc_expr'

and fill_public venv expr =
  let fill v =
    match Hashtbl.find venv v with
      | VarEntry lt when lt.label = None -> update_label venv v (Some Public)
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
        ignore(List.map (function
                          | ValArg e -> fill_public venv e
                          | VarArg(_,v) -> fill v
                          | ArrArg(_,v) -> fill v)
                 args)

and ty_can_flow lhs rhs =
  match lhs, rhs with
    | _ when lhs = rhs -> ()
    | Int a, Int b when a > b -> ()
    | Int _, NumericBottom -> ()
    | UInt a, UInt b when a > b -> ()
    | UInt _, NumericBottom -> ()
    | _ -> raise errPassError

and lbl_can_flow venv lt e =
  match lt.label, e.label with
    | Some Public, Some Secret -> raise errFlowError
    | Some Public, None -> fill_public venv e; lt
    | None, Some Secret -> { lt with label=(Some Secret) }
    | _ -> lt

and can_flow venv lt expr =
  ty_can_flow lt.ty expr.ty;
  lbl_can_flow venv lt expr

and tc_stm venv fn_ty lbl_ctx { pos=p; data=stm } =
  let unify_ctx' e = { e with label=(unify_label e.pos lbl_ctx e.label) } in
  let unify_ctx = repack unify_ctx' in
  match stm with
  | VarDec(name,lt,expr) ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let lt' = can_flow venv lt expr' in
      Hashtbl.add venv name (VarEntry lt');
      VarDec(name,lt',expr'), Public
  | Assign(name,expr) ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (errVarNotDefined name p) in
      (match v with
        | VarEntry { kind=Val } ->
          raise (TypeError("Cannot assign to read-only variable @ " ^ pos_string p))
        | VarEntry lt ->
          let lt' = can_flow venv lt expr' in
            if lt.label = None then update_label venv name lt'.label;
            Assign(name,expr'), Public
        | _ -> raise (errFoundNotVar name p))
  | ArrAssign(name,index,expr) ->
    let index' = tc_expr venv index in
    let public_num = { ty=NumericTop; label=Some Public } in
    let _ = can_flow venv public_num index' in
    let expr' = unify_ctx (tc_expr venv expr) in
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (errVarNotDefined name p) in
      (match v with
        | VarEntry { kind=Val } ->
          raise (TypeError("Cannot assign to read-only array @ " ^ pos_string p))
        | VarEntry ({ ty=(Array a); kind=_ } as lt) ->
          let lte = { lt with ty:a.ty } in
          let lte' = can_flow venv lte expr' in
          let lt' = { lt with label=lte'.label } in
            if lt.label = None then update_label venv name lt'.label;
            ArrAssign(name,index',expr'), Public
        | _ -> raise (errFoundNotVar name p))
  | If(cond,tstms,fstms) ->
    (* TODO: implement 2 if statements in the core language:
       a constant if and non constant if. *)
    let cond' = tc_expr venv cond in
    ignore(ty_can_flow Bool cond'.ty);
    let lbl_ctx' = unify_ctx cond'.label in
    let tstms',ctx_t = tc_block (Hashtbl.copy venv) fn_ty lbl_ctx' tstms in
    let fstms',ctx_f = tc_block (Hashtbl.copy venv) fn_ty lbl_ctx' fstms in
      If(cond',tstms',fstms'), (unify_label ctx_t ctx_f)
  | For(name,ty,l,h,body) ->
    let l' = tc_expr venv l in
    let h' = tc_expr venv h in
    let lt = { ty=ty; label=Public } in
    ignore(can_flow venv lt l');
    ignore(can_flow venv lt h');
    let venv' = Hashtbl.copy venv in
      Hashtbl.add venv' name (VarEntry { ty=ty; label=Public; kind=Val });
      let body' = tc_block venv' fn_ty lbl_ctx body in
        For(name,ty,l',h',body'), Public
  | Return expr ->
    let expr' = unify_ctx (tc_expr venv expr) in
    let fn_ty' = can_flow venv fn_ty expr' in
      Return expr', lbl_ctx

and tc_stms venv fn_ty lbl_ctx = function
  | [] -> [], lbl_ctx
  | stm::stms ->
    let stm',lbl_ctx' = tc_stm venv fn_ty lbl_ctx in
    let lbl_ctx' = unify_label lbl_ctx lbl_ctx' in
    let stms' = tc_stms venv fn_ty lbl_ctx' stms in
      stm'::stms', lbl_ctx'


and tc_block venv fn_ty lbl_ctx stms =
  let stms',_ = tc_stms venv fn_ty lbl_ctx stms in
    ignore(set_missing_labels_to_public venv);
    tc_stms venv fn_ty lbl_ctx stms'

and tc_fdec venv { pos; data=fdec } =
  match fdec with
    | { rty={ ty=(Array _) } } ->
      raise (TypeError ("Function cannot return an array @ " ^ pos_string pos))
    | _ ->
      let venv' = Hashtbl.copy venv in
      let args_ty = List.map (fun { name=n; lt=lt } ->
                               Hashtbl.add venv' n (VarEntry lt); lt)
                      args in
      let body',_ = tc_block venv' fdec.rty Public body' in
      let fdec' = { fdec with body=body' } in
        Hashtbl.add venv name (FunEntry { f_ty=fdec'.rty; f_args=args_ty });
        fdec'

and tc_module (CModule l) =
  ignore(List.map (tc_fdec Env.venv) l)
