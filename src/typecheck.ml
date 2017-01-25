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

let unify t t1 p =
  match (t,t1) with
  | (Int32,Int32) -> Int32
  | (Int32,Int16) -> Int32
  | (Int16,Int32) -> Int32
  | (Int32,Int8) -> Int32
  | (Int8,Int32) -> Int32
  | (Int16,Int16) -> Int16
  | (Int16,Int8) -> Int16
  | (Int8,Int16) -> Int16
  | (Int8,Int8) -> Int8
  | (Bool,Bool) -> Bool
  | (ByteArr x, ByteArr y) when x = y -> (ByteArr x)
  | _ -> raise (TypeError(ty_to_string(t) ^ " does not unify with "
                ^ ty_to_string(t1) ^ " @ " ^ (pos_string p)))

let unify_lt lt lt' p =
  match lt,lt' with
    | { ty=t; label=Some Public }, { ty=t'; label=Some Public } ->
      { ty=(unify t t' p); label=Some Public; kind=Val }
    | { ty=t; label=Some Secret }, { ty=t'; label=Some Public } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=Some Public }, { ty=t'; label=Some Secret } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=Some Secret }, { ty=t'; label=Some Secret } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=None }, { ty=t'; label=Some Secret } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=Some Secret }, { ty=t'; label=None } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=None }, { ty=t'; label=l } ->
      { ty=(unify t t' p); label=None; kind=Val }
    | { ty=t; label=l }, { ty=t'; label=None } ->
      { ty=(unify t t' p); label=None; kind=Val }

(* The `can_flow_to` operation that returns the unified label and handles
   label inference *)
let unify_flows_to ~ret_label ~expr_label p =
  match ret_label, expr_label with
    | { ty=t; label=Some Public }, { ty=t'; label=Some Public } ->
      { ty=(unify t t' p); label=Some Public; kind=Val }
    | { ty=t; label=Some Public }, { ty=t'; label=Some Secret } ->
      raise (TypeError ("A secret value cannot flow to a public value @ " ^
              (pos_string p)))
    | { ty=t; label=Some Secret }, { ty=t'; label=Some Public } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=Some Secret }, { ty=t'; label=Some Secret } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=None }, { ty=t'; label=Some Public } ->
      { ty=(unify t t' p); label=Some Public; kind=Val }
    | { ty=t; label=None }, { ty=t'; label=Some Secret } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=Some Public }, { ty=t'; label=None } ->
      { ty=(unify t t' p); label=Some Public; kind=Val }
    | { ty=t; label=Some Secret }, { ty=t'; label=None } ->
      { ty=(unify t t' p); label=Some Secret; kind=Val }
    | { ty=t; label=None }, { ty=t'; label=None } ->
      { ty=(unify t t' p); label=None; kind=Val }

(* Unifies the labeled types of args to operators *)
let unify_op (rt,arg_ts) ts p =
  let unify_op' t t' =
    match t,t' with
      | t,{ ty=t'; label=l } -> (unify t t' p) in
  ignore(List.map2 (fun t t1 -> unify_op' t t1) arg_ts ts);
  rt

(* Unifies function args during a function call *)
let unify_fn_args arg_ts ts p =
  List.map2 (fun t t' -> unify_lt t t' p) arg_ts ts

let isInt = function
  | Int32 -> true
  | Int16 -> true
  | Int8 -> true
  | _ -> false

let isBool = function
  | Bool -> true
  | _ -> false

let rec tc_unop = function
  | Neg _ -> (Int32, [Int32])
  | L_Not _ -> (Bool, [Bool])
  | B_Not _ -> (Int32, [Int32])

and op_arg = function
  | Plus _ -> Int8
  | Minus _ -> Int8
  | Multiply _ -> Int8
  | Equal _ -> Int8
  | NEqual _ -> Int8
  | GT _ -> Int8
  | GTE _ -> Int8
  | LT _ -> Int8
  | LTE _ -> Int8
  | L_And _ -> Bool
  | L_Or _ -> Bool
  | B_And _ -> Int8
  | B_Or _ -> Int8
  | B_Xor _ -> Int8
  | LeftShift _ -> Int8
  | RightShift _ -> Int8

and tc_binop lhs rhs p = function
  | Plus p when isInt(lhs) && isInt(rhs) -> unify lhs rhs p
  | Minus p when isInt(lhs) && isInt(rhs) -> unify lhs rhs p
  | Multiply p when isInt(lhs) && isInt(rhs) -> unify lhs rhs p
  | Equal _ when isInt(lhs) && isInt(rhs) -> Bool
  | NEqual _ when isInt(lhs) && isInt(rhs) -> Bool
  | GT p when isInt(lhs) && isInt(rhs) -> Bool
  | GTE p when isInt(lhs) && isInt(rhs) -> Bool
  | LT p when isInt(lhs) && isInt(rhs) -> Bool
  | LTE p when isInt(lhs) && isInt(rhs) -> Bool
  | L_And p when isBool(lhs) && isBool(rhs) -> Bool
  | L_Or p when isBool(lhs) && isBool(rhs) -> Bool
  | B_And p when isInt(lhs) && isInt(rhs) -> unify lhs rhs p
  | B_Or p when isInt(lhs) && isInt(rhs) -> unify lhs rhs p
  | B_Xor p when isInt(lhs) && isInt(rhs) -> unify lhs rhs p
  | LeftShift p when isInt(lhs) && isInt(rhs) && 
                     (equal_ctype lhs (unify lhs rhs p)) -> lhs
  | RightShift p when isInt(lhs) && isInt(rhs) &&
                      (equal_ctype lhs (unify lhs rhs p)) -> lhs
  | _ -> raise (TypeError("Types cannot be unified for given operator @ " ^
          (pos_string p)))

and tc_prim lhs = function
  | Number n -> 
    begin
      match lhs.ty with
        | ByteArr n -> { ty=Int32; label=lhs.label; kind=Val }
        | ty -> { ty=ty; label=lhs.label; kind=Val }
    end
  | ByteArray s -> { ty=(ByteArr (List.length s)); label=lhs.label; kind=Ref }
  | Boolean b -> { ty=Bool; label=lhs.label; kind=Val }

and tc_expr venv lhs_lt = function
  | VarExp(v,p) ->
    (try
       match Hashtbl.find venv v with
       | VarEntry { v_ty={ ty=t; label=None } } ->
         update_label v venv lhs_lt.label
       | VarEntry { v_ty=ty } -> ty
       | LoopEntry { v_ty=ty } -> ty
       | StaticVarEntry { v_ty=ty } -> ty
       | _ -> raise (VariableNotDefined("Variable `" ^ v ^ "` not defined"
                           ^ (pos_string p)))
     with
       Not_found -> raise (VariableNotDefined("Variable `" ^ v ^ "` not defined"
                           ^ (pos_string p))))
  | ArrExp(v,i,p) ->
    let ret = try Hashtbl.find venv v with 
      | Not_found -> raise (VariableNotDefined("Variable `" ^ v ^ "` not defined"
          ^ (pos_string p))) in
    let ret_label = (match ret with
      | VarEntry { v_ty={ ty=t; label=l } } -> l
      | StaticVarEntry { v_ty={ ty=t; label=l } } -> l
      | LoopEntry _ -> raise (InternalCompilerError("Found a loop entry in " ^
                              " place of var entry(array)"))
      | FunEntry _ -> raise (InternalCompilerError("Found a fun entry in " ^
      " place of var entry(array)"))) in
    (match i with
     | Primitive _ -> { ty=Int32; label=ret_label; kind=Val }
     | VarExp(n,p) ->
       (try
          match Hashtbl.find venv n with
          | LoopEntry { v_ty={ ty=Int32; label=Some Public} } ->
            { ty=Int32; label=ret_label; kind=Val }
          | LoopEntry { v_ty={ ty=t; label=Some Secret } } ->
            raise (InternalCompilerError ("Loop variables cannot be secret"))
          | LoopEntry { v_ty={ ty=t; label=l } } ->
            raise (InternalCompilerError ("Loop variable must be public ints"))
          | VarEntry { v_ty={ ty=t; label=None } } ->
            ignore(update_label n venv (Some Public));
            { ty=Int32; label=ret_label; kind=Val }
          | VarEntry { v_ty={ ty=Int32; label=Some Public } } ->
            { ty=Int32; label=ret_label; kind=Val }
          | VarEntry { v_ty={ ty=t; label=Some Secret } } ->
            raise (TypeError
              ("Arrays cannot be accessed with a secret variable @ " ^ 
              (pos_string p)))
          | StaticVarEntry { v_ty={ ty=t; label=None } } ->
            ignore(update_label n venv (Some Public));
            { ty=Int32; label=ret_label; kind=Val }
          | StaticVarEntry { v_ty={ ty=Int32; label=Some Public } } ->
            { ty=Int32; label=ret_label; kind=Val }
          | StaticVarEntry { v_ty={ ty=t; label=Some Secret } } ->
            raise (TypeError
              ("Arrays cannot be accessed with a secret variable @ " ^ 
              (pos_string p)))
          | StaticVarEntry { v_ty={ ty=t; label=l } } ->
            raise (TypeError "Arrays can only be accessed with public ints")
          | VarEntry { v_ty={ ty=t; label=l } } ->
            raise (TypeError "Arrays can only be accessed with public ints")
          | FunEntry _ -> raise (TypeError ("Cannot access an array with a " ^
                            "function yet @ " ^ (pos_string p)))
       with
       | Not_found -> raise (VariableNotDefined("Variable `" ^ n ^
                             "` not defined @ " ^ (pos_string p))))
     | _ -> raise (TypeError ("Arrays can only be accessed with constant " ^
                      "numbers or loop variables @ " ^ (pos_string p))))
  | UnOp(op,expr,p) ->
    let op_ty = tc_unop op in
    let expr_ty = tc_expr venv lhs_lt expr in
    ignore(unify_op op_ty [expr_ty] p);
    expr_ty
  | BinOp(op,expr1,expr2,p) ->
    let arg_ty = { lhs_lt with ty=op_arg op } in
    let lhs = tc_expr venv arg_ty expr1 in
    let rhs = tc_expr venv arg_ty expr2 in
    let rt = tc_binop lhs.ty rhs.ty p op in
    { ty=rt; label=(unify_lt lhs rhs p).label; kind=Val }
  | Primitive(p,_) -> tc_prim lhs_lt p
  | CallExp(name,args,p) ->
    (try
       match Hashtbl.find venv name with
       | VarEntry _ ->
         raise (CallError ("Unable to call variable `" ^ name ^ "` @ "
                ^ (pos_string p)))
       | LoopEntry _ ->
         raise (CallError ("Unable to call loop variable `" ^ name ^ "` @ "
                ^ (pos_string p)))
       | StaticVarEntry _ ->
         raise (CallError ("Unable to call static variable `" ^ name ^ "` @ "
                ^ (pos_string p)))
       | FunEntry { f_ty=ty; f_args=args' } ->
         ignore(unify_fn_args args' (List.map (tc_expr venv lhs_lt) args) p);
         ty
     with
       Not_found -> raise (FunctionNotDefined("Function, `" ^ name ^
                           "`, not defined at function call @ " ^
                           (pos_string p))))

and tc_stm fn_ty venv f_name = function
  | VarDec(name,lt,expr,p) ->
    let expr_ty = tc_expr venv lt expr in
    let lt = unify_flows_to lt expr_ty p in
    (match lt with 
      | { kind=Val; ty=(ByteArr n)} ->
        raise (TypeError ("Byte arrays must be of `ref` type @ " ^
          (pos_string p)))
      | _ -> ());
    Hashtbl.add venv name (VarEntry { v_ty=lt });
  | Assign(name,expr,p) ->
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (VariableNotDefined("Variable, `" ^ name ^
                            "`, not defined @ " ^ (pos_string p))) in
    (match v with
     | VarEntry { v_ty={ ty=t; label=Some Secret } as lt } ->
       let lt_expr = tc_expr venv lt expr in
       ignore(unify_flows_to lt lt_expr p);
     | VarEntry { v_ty={ ty=t; label=Some Public } as lt} ->
       let lt_expr = tc_expr venv lt expr in
       ignore(unify_flows_to lt lt_expr p);
     | VarEntry { v_ty={ ty=t; label=None } as lt } ->
       let lt_expr = tc_expr venv lt expr in
       let { ty=t; label=l } =
        (unify_lt lt lt_expr p) in
       ignore(update_label name venv l)
     | StaticVarEntry _ ->
         raise (TypeError("Cannot assign a static variable @ " ^ pos_string p))
     | _ -> raise (VariableNotDefined("Variable, `" ^ name ^ "`, not defined @ "
                                      ^ (pos_string p))))
  | ArrAssign(name,index,expr,p) ->
    let public_int = { ty=Int32; label=Some Public; kind=Val } in
    let index_ty = tc_expr venv public_int index in
    (try
      (match Hashtbl.find venv name with
       | VarEntry { v_ty={ ty=(ByteArr x); label=Some Public } as lt } ->
         let expr_ty = tc_expr venv lt expr in
         ignore(unify_flows_to public_int expr_ty p);
         ignore(unify_flows_to public_int index_ty p);
       | VarEntry { v_ty={ ty=(ByteArr x); label=Some Secret } as lt } ->
         let private_int = { ty=Int32; label=Some Secret; kind=Val } in
         let expr_ty = tc_expr venv lt expr in
         ignore(unify_flows_to private_int expr_ty p);
         ignore(unify_flows_to public_int index_ty p);
       | VarEntry { v_ty={ ty=(ByteArr x); label=None } as lt } ->
         let expr_ty = tc_expr venv lt expr in
         let { ty=t; label=l } =
          unify_flows_to
            { ty=(ByteArr x); label=None; kind=Ref } expr_ty p in
         ignore(update_label name venv l)
       | StaticVarEntry _ ->
         raise (TypeError("Cannot assign a static variable @ " ^ pos_string p))
       | _ -> raise (VariableNotDefined("Variable, `" ^ name ^ 
                     "`, not defined @ " ^ (pos_string p))))
    with
      Not_found -> raise (VariableNotDefined("Variable, `" ^ name ^
                          "`, not defined @ " ^ (pos_string p))))
  | If(cond,then',else',p) ->
    (* TODO: This can be the place of an optimization. Rather than unify to
       a private label, get the actual label. Then implement 2 if statements
       in the core language: a constant if and non constant if. *)
    (* NOTE: Should we add a new syntax for labeling expressions? This could
       be useful for things wthout a name. For instance, conside 
       private bytearr[10].
       bytearr[0] = Secret 1 -- right now we have to declare 1, but this
       seems like a nicer syntax
    *)
    let none_bool = { ty=Bool; label=None; kind=Val } in
    let cond_lt = tc_expr venv none_bool cond in
    ignore(unify Bool cond_lt.ty p);
    ignore(tc_stms fn_ty venv then' f_name);
    ignore(tc_stms fn_ty venv else' f_name);
  | For(name,l,h,body,p) ->
    (* TODO: Same as if statements *)
    let public_int = { ty=Int32; label=Some Public; kind=Val } in
    ignore(unify_lt (tc_expr venv public_int (Primitive(l,None))) public_int p);
    ignore(unify_lt (tc_expr venv public_int (Primitive(h,None))) public_int p);
    (match (l,h) with
     | (Number l', Number h') ->
       if l' >= h' then raise
           (ForError ("Low value must be smaller than high value in for loop @ "
                      ^ (pos_string p)))
     | _ ->
       raise (TypeError ("Low and high values must be integers in for loop @ "
                         ^ (pos_string p))));
    let _ = Hashtbl.add venv name (LoopEntry { v_ty=public_int }) in
    tc_stms fn_ty venv body f_name
  | Return(expr,p) ->
    let exp_label = tc_expr venv fn_ty expr in
    let fn_ty' = (match fn_ty with
      | { ty=_; label=None } ->
        save_fn_ret_label exp_label f_name;
        exp_label
      | _ -> fn_ty) in
    ignore(unify_flows_to fn_ty' exp_label p)

and tc_stms fn_ty venv stms f_name =
  ignore(List.map (tc_stm fn_ty venv f_name) stms)

and tc_fdec venv = function
  (* TODO: This needs to return a list of environments.
     The next stage in the compilation pipeline should be to rewrite the AST
     to reflect the inferred labels. It bears no effect on the output
     assembly as of right now, but the plan is for it to by having constant
     and non-constant time branching/loops/etc. This can only be done with
     an up to date and accurate AST. *)
  | FunctionDec(_,_,{ ty=ByteArr(_); label=_ },_,p) ->
    raise (TypeError("Functions cannot return a ByteArray @ " ^ (pos_string p)))
  | FunctionDec(name,args,ty,body,p) ->
    let rewrite_arg = function
      | { name=n; lt={ ty=t; label=None; kind=k }; p=p } ->
        { name=n; lt={ ty=t; label=Some Secret; kind=k }; p=p }
      | arg -> arg in
    let args' = List.map rewrite_arg args in
    let venv' = Hashtbl.copy venv in
    let create_entry = function
      | { kind=Val } as lt -> StaticVarEntry { v_ty=lt }
      | { kind=Ref } as lt -> VarEntry { v_ty=lt }
      | { kind=Out } as lt -> VarEntry { v_ty=lt } in
    let tc_arg = function
      | { kind=Val; ty=(ByteArr n) } ->
        raise (TypeError("Byte arrays must be of `ref` types @ " ^
          (pos_string p)))
      | arg -> arg in
    let args_ty = List.map (fun { name=n; lt=lt } ->
                              Hashtbl.add venv' n (create_entry (tc_arg lt));
                              lt)
                    args' in
    let _ = tc_stms ty venv' body name in
    let lt = get_fn_ret_label ~default:ty name in
    Hashtbl.add venv name (FunEntry { f_ty=lt; f_args=args_ty });
    default_to_secret venv'

and tc_module (CModule l) =
  List.fold_left (fun a f -> ignore(tc_fdec Env.venv f)) () l
