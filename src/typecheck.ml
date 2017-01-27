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

type typed_exp = { ty':labeled_type; exp: expr }

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
  | (Int,Int32) -> Int32
  | (Int,Int16) -> Int16
  | (Int,Int8) -> Int8
  | (Int32,Int) -> Int32
  | (Int16,Int) -> Int16
  | (Int8,Int) -> Int8
  | (UInt32,UInt32) -> UInt32
  | (UInt32,UInt16) -> UInt32
  | (UInt16,UInt32) -> UInt32
  | (UInt32,UInt8) -> UInt32
  | (UInt8,UInt32) -> UInt32
  | (UInt16,UInt16) -> UInt16
  | (UInt16,UInt8) -> UInt16
  | (UInt8,UInt16) -> UInt16
  | (UInt8,UInt8) -> UInt8
  | (Int,UInt32) -> UInt32
  | (Int,UInt16) -> UInt16
  | (Int,UInt8) -> UInt8
  | (UInt32,Int) -> UInt32
  | (UInt16,Int) -> UInt16
  | (UInt8,Int) -> UInt8
  | (Bool,Bool) -> Bool
  | (Int,Int) -> Int
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
  | UInt32 -> true
  | UInt16 -> true
  | UInt8 -> true
  | Int -> true
  | _ -> false

let isBool = function
  | Bool -> true
  | _ -> false

let rec tc_unop = function
  | Neg _ -> (Int32, [Int32])
  | L_Not _ -> (Bool, [Bool])
  | B_Not _ -> (Int32, [Int32])

and op_arg = function
  | Plus _ -> Int
  | Minus _ -> Int
  | Multiply _ -> Int
  | Equal _ -> Int
  | NEqual _ -> Int
  | GT _ -> Int
  | GTE _ -> Int
  | LT _ -> Int
  | LTE _ -> Int
  | L_And _ -> Bool
  | L_Or _ -> Bool
  | B_And _ -> Int
  | B_Or _ -> Int
  | B_Xor _ -> Int
  | LeftShift _ -> Int
  | RightShift _ -> Int

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
  | LeftShift p when isInt(lhs) && isInt(rhs) -> lhs
  | RightShift p when isInt(lhs) && isInt(rhs) -> lhs
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
  | VarExp(v,p) as e ->
    (try
       match Hashtbl.find venv v with
       | VarEntry { v_ty={ ty=t; label=None } } ->
         let lt = update_label v venv lhs_lt.label in
         { ty'=lt; exp=e }
       | VarEntry { v_ty=ty } -> { ty'=ty; exp=e }
       | LoopEntry { v_ty=ty } -> { ty'=ty; exp=e }
       | StaticVarEntry { v_ty=ty } -> { ty'=ty; exp=e }
       | _ -> raise (VariableNotDefined("Variable `" ^ v ^ "` not defined"
                           ^ (pos_string p)))
     with
       Not_found -> raise (VariableNotDefined("Variable `" ^ v ^ "` not defined"
                           ^ (pos_string p))))
  | ArrExp(v,i,p) as e ->
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
     | Primitive _ -> { ty'={ ty=Int32; label=ret_label; kind=Val }; exp=e }
     | VarExp(n,p) ->
       (try
          match Hashtbl.find venv n with
          | LoopEntry { v_ty={ ty=Int32; label=Some Public} } ->
            { ty'={ ty=Int32; label=ret_label; kind=Val }; exp=e }
          | LoopEntry { v_ty={ ty=t; label=Some Secret } } ->
            raise (InternalCompilerError ("Loop variables cannot be secret"))
          | LoopEntry { v_ty={ ty=t; label=l } } ->
            raise (InternalCompilerError ("Loop variable must be public ints"))
          | VarEntry { v_ty={ ty=t; label=None } } ->
            ignore(update_label n venv (Some Public));
            { ty'={ ty=Int32; label=ret_label; kind=Val }; exp=e }
          | VarEntry { v_ty={ ty=Int32; label=Some Public } } ->
            { ty'={ ty=Int32; label=ret_label; kind=Val }; exp=e }
          | VarEntry { v_ty={ ty=t; label=Some Secret } } ->
            raise (TypeError
              ("Arrays cannot be accessed with a secret variable @ " ^ 
              (pos_string p)))
          | StaticVarEntry { v_ty={ ty=t; label=None } } ->
            ignore(update_label n venv (Some Public));
            { ty'={ ty=Int32; label=ret_label; kind=Val }; exp=e }
          | StaticVarEntry { v_ty={ ty=Int32; label=Some Public } } ->
            { ty'={ ty=Int32; label=ret_label; kind=Val }; exp=e }
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
  | UnOp(op,expr,p) as e ->
    let op_ty = tc_unop op in
    let expr_ty = tc_expr venv lhs_lt expr in
    ignore(unify_op op_ty [expr_ty.ty'] p);
    { ty'=expr_ty.ty'; exp=e }
  | BinOp(op,expr1,expr2,p) ->
    let arg_ty = { lhs_lt with ty=op_arg op } in
    let lhs = tc_expr venv arg_ty expr1 in
    let rhs = tc_expr venv arg_ty expr2 in
    let rt = tc_binop lhs.ty'.ty rhs.ty'.ty p op in
    let e' = BinOp(op,lhs.exp,rhs.exp,p) in
    let label = (unify_lt lhs.ty' rhs.ty' p).label in
    { ty'={ ty=rt; label=label; kind=Val }; exp=e' }
  | Primitive(p,_) as e -> { ty'=(tc_prim lhs_lt p); exp=e }
  | CallExp(name,args,p) as e ->
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
         let args'' = List.map (tc_expr venv lhs_lt) args in
         ignore(unify_fn_args args' (List.map (fun arg -> arg.ty') args'') p);
         { ty'=ty; exp=e }
     with
       Not_found -> raise (FunctionNotDefined("Function, `" ^ name ^
                           "`, not defined at function call @ " ^
                           (pos_string p))))

and tc_stm fn_ty venv f_name = function
  | VarDec(name,lt,expr,p) ->
    let expr' = tc_expr venv lt expr in
    let lt = unify_flows_to lt expr'.ty' p in
    (match lt with 
      | { kind=Val; ty=(ByteArr n)} ->
        raise (TypeError ("Byte arrays must be of `ref` type @ " ^
          (pos_string p)))
      | _ -> ());
    Hashtbl.add venv name (VarEntry { v_ty=lt });
    VarDec(name,expr'.ty',expr'.exp,p)
  | Assign(name,expr,p) ->
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (VariableNotDefined("Variable, `" ^ name ^
                            "`, not defined @ " ^ (pos_string p))) in
    (match v with
     | VarEntry { v_ty={ ty=t; label=Some Secret } as lt } ->
       let expr' = tc_expr venv lt expr in
       ignore(unify_flows_to lt expr'.ty' p);
       Assign(name,expr'.exp,p)
     | VarEntry { v_ty={ ty=t; label=Some Public } as lt} ->
       let expr' = tc_expr venv lt expr in
       ignore(unify_flows_to lt expr'.ty' p);
       Assign(name,expr'.exp,p)
     | VarEntry { v_ty={ ty=t; label=None } as lt } ->
       let expr' = tc_expr venv lt expr in
       let { ty=t; label=l } =
        (unify_lt lt expr'.ty' p) in
       ignore(update_label name venv l);
       Assign(name,expr'.exp,p)
     | StaticVarEntry _ ->
         raise (TypeError("Cannot assign a static variable @ " ^ pos_string p))
     | _ -> raise (VariableNotDefined("Variable, `" ^ name ^ "`, not defined @ "
                                      ^ (pos_string p))))
  | ArrAssign(name,index,expr,p) ->
    let public_int = { ty=Int32; label=Some Public; kind=Val } in
    let index = tc_expr venv public_int index in
    (try
      (match Hashtbl.find venv name with
       | VarEntry { v_ty={ ty=(ByteArr x); label=Some Public } as lt } ->
         let expr' = tc_expr venv lt expr in
         ignore(unify_flows_to public_int expr'.ty' p);
         ignore(unify_flows_to public_int index.ty' p);
         ArrAssign(name,index.exp,expr'.exp,p)
       | VarEntry { v_ty={ ty=(ByteArr x); label=Some Secret } as lt } ->
         let private_int = { ty=Int32; label=Some Secret; kind=Val } in
         let expr' = tc_expr venv lt expr in
         ignore(unify_flows_to private_int expr'.ty' p);
         ignore(unify_flows_to public_int index.ty' p);
         ArrAssign(name,index.exp,expr'.exp,p)
       | VarEntry { v_ty={ ty=(ByteArr x); label=None } as lt } ->
         let expr' = tc_expr venv lt expr in
         let { ty=t; label=l } =
          unify_flows_to
            { ty=(ByteArr x); label=None; kind=Ref } expr'.ty' p in
         ignore(update_label name venv l);
         ArrAssign(name,index.exp,expr'.exp,p)
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
    let cond' = tc_expr venv none_bool cond in
    ignore(unify Bool cond'.ty'.ty p);
    let then'' = tc_stms fn_ty venv then' f_name in
    let else'' = tc_stms fn_ty venv else' f_name in
    If(cond'.exp,then'',else'',p)
  | For(name,l,h,body,p) ->
    (* TODO: Same as if statements *)
    let public_int = { ty=Int32; label=Some Public; kind=Val } in
    ignore(unify_lt
      (tc_expr venv public_int (Primitive(l,None))).ty' public_int p);
    ignore(unify_lt
      (tc_expr venv public_int (Primitive(h,None))).ty' public_int p);
    (match (l,h) with
     | (Number l', Number h') ->
       if l' >= h' then raise
           (ForError ("Low value must be smaller than high value in for loop @ "
                      ^ (pos_string p)))
     | _ ->
       raise (TypeError ("Low and high values must be integers in for loop @ "
                         ^ (pos_string p))));
    let _ = Hashtbl.add venv name (LoopEntry { v_ty=public_int }) in
    let body' = tc_stms fn_ty venv body f_name in
    For(name,l,h,body',p)
  | Return(expr,p) ->
    let expr = tc_expr venv fn_ty expr in
    let fn_ty' = (match fn_ty with
      | { ty=_; label=None } ->
        let lt = { fn_ty with label=expr.ty'.label } in
        save_fn_ret_label lt f_name;
        lt
      | _ -> fn_ty) in
    ignore(unify fn_ty'.ty expr.ty'.ty p);
    ignore(unify_flows_to fn_ty' expr.ty' p);
    Return(expr.exp,p)

and tc_stms fn_ty venv stms f_name =
  List.map (tc_stm fn_ty venv f_name) stms

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
    let body' = tc_stms ty venv' body name in
    let lt = get_fn_ret_label ~default:ty name in
    Hashtbl.add venv name (FunEntry { f_ty=lt; f_args=args_ty });
    default_to_secret venv';
    FunctionDec(name,args,ty,body',p)

and tc_module (CModule l) =
  CModule (List.map (tc_fdec Env.venv) l)
