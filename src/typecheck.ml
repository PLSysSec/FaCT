open Ast
open Env

exception NotImplemented
exception VariableNotDefined of string
exception FunctionNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string

let unify t t1 p =
  match (t,t1) with
  | (Int,Int) -> Int
  | (Bool,Bool) -> Bool
  | (ByteArr x, ByteArr y) when x = y -> (ByteArr x)
  | _ -> raise (TypeError(ty_to_string(t) ^ " does not unify with "
                ^ ty_to_string(t1) ^ " @ " ^ (pos_string p)))

let unify_equal_lt lt lt' p =
  match lt,lt' with
    | Public t, Public t' -> Public(unify t t' p)
    | Private t, Private t' -> Private(unify t t' p)
    | _ -> raise (TypeError ("Labels are expected to be the same @ " ^
                  (pos_string p)))

let unify_lt lt lt' p =
  match lt,lt' with
    | Public t, Public t' -> Public(unify t t' p)
    | Private t, Public t' -> Private(unify t t' p)
    | Public t, Private t' -> Private(unify t t' p)
    | Private t, Private t' -> Private(unify t t' p)

let unify_fn (rt,arg_ts) ts p =
  let unify_fn' t t' =
    match t,t' with
      | t,Public t' -> unify t t' p
      | t,Private t' -> unify t t' p in
  ignore(List.map2 (fun t t1 -> unify_fn' t t1) arg_ts ts);
  rt

let unify_fn_args arg_ts ts p =
  List.map2 (fun t t' -> unify_lt t t' p) arg_ts ts

let rec tc_unop = function
  | B_Not _ -> (Int, [Int])

and tc_binop = function
  | Plus _ -> (Int, [Int;Int])
  | Minus _ -> (Int, [Int;Int])
  | Multiply _ -> (Int, [Int;Int])
  | Equal _ -> (Bool, [Int;Int])
  | NEqual _ -> (Bool, [Int;Int])
  | GT _ -> (Bool, [Int;Int])
  | GTE _ -> (Bool, [Int;Int])
  | LT _ -> (Bool, [Int;Int])
  | LTE _ -> (Bool, [Int;Int])
  | B_And _ -> (Int, [Int;Int])
  | B_Or _ -> (Int, [Int;Int])
  | LeftShift _ -> (Int, [Int;Int])
  | RightShift _ -> (Int, [Int;Int])

and tc_prim = function
  | Number n -> Public Int
  | ByteArray s -> Public(ByteArr (List.length s))
  | Boolean b -> Public Bool

and tc_expr venv = function
  | VarExp(v,p) ->
    (try
       match Hashtbl.find venv v with
       | VarEntry { v_ty=ty } -> ty
       | LoopEntry { v_ty=ty } -> ty
       | _ -> raise (VariableNotDefined(v))
     with
       Not_found -> raise (VariableNotDefined("Variable `" ^ v ^ "` not defined"
                           ^ (pos_string p))))
  | ArrExp(v,i,p) ->
    (match i with
     | Primitive _ -> Public Int
     | VarExp(n,p) ->
       (try
          match Hashtbl.find venv n with
          | LoopEntry { v_ty=(Public ty) } -> Public ty
          | LoopEntry { v_ty=(Private ty) } ->
            raise (TypeError ("Arrays cannot be accessed with a private " ^ 
            "variable @ " ^ (pos_string p)))
          | _ -> raise (TypeError ("Arrays can only be accessed with " ^
                        "constant numbers, loop variables, or public " ^
                        "variables" ^ (pos_string p)))
       with
       | Not_found -> raise (VariableNotDefined("Variable `" ^ n ^
                             "` not defined @ " ^ (pos_string p)) ))
     | _ -> raise (TypeError ("Arrays can only be accessed with constant " ^
                              "numbers or loop variables @ " ^ (pos_string p))))
  | UnOp(op,expr,p) ->
    let op_ty = tc_unop op in
    let expr_ty = tc_expr venv expr in
    ignore(unify_fn op_ty [expr_ty] p);
    expr_ty
  | BinOp(op,expr1,expr2,p) ->
    let (rt,args) as op_ty = tc_binop op in
    let expr1_ty = tc_expr venv expr1 in
    let expr2_ty = tc_expr venv expr2 in
    ignore(unify_fn op_ty [expr1_ty;expr2_ty] p);
    (match unify_lt expr1_ty expr2_ty p with
      | Public _ -> Public rt
      | Private _ -> Private rt)
  | Primitive(p,_) -> tc_prim p
  | CallExp(name,args,p) ->
    (try
       match Hashtbl.find venv name with
       | VarEntry _ ->
         raise (CallError ("Unable to call variable `" ^ name ^ "` @ "
                ^ (pos_string p)))
       | LoopEntry _ ->
         raise (CallError ("Unable to call loop variable `" ^ name ^ "` @ "
                ^ (pos_string p)))
       | FunEntry { f_ty=ty; f_args=args' } ->
         ignore(unify_fn_args args' (List.map (tc_expr venv) args) p);
         Public(ty)
     with
       Not_found -> raise (FunctionNotDefined("Function, `" ^ name ^
                           "`, not defined at function call @ " ^
                           (pos_string p))))

and tc_stm fn_ty venv = function
  | VarDec(name,lt,expr,p) ->
    let expr_ty = tc_expr venv expr in
    let lt = unify_lt lt expr_ty p in
    Hashtbl.add venv name (VarEntry { v_ty=lt })
  | Assign(name,expr,p) ->
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (VariableNotDefined("Variable, `" ^ name ^
                            "`, not defined @ " ^ (pos_string p))) in
    (match v with
     | VarEntry { v_ty=ty } ->
       ignore(unify_lt ty (tc_expr venv expr) p)
     | _ -> raise (VariableNotDefined("Variable, `" ^ name ^ "`, not defined @ "
                                      ^ (pos_string p))))
  | ArrAssign(name,index,expr,p) ->
    (* TODO: What is the expected behavior of this?
       Right now, the index must be a Public Int and the value being stored
       must have the same labeled type as the array *)
    let index_ty = tc_expr venv index in
    ignore(unify_equal_lt (Public Int) index_ty p);
    (try
      (match Hashtbl.find venv name with
       | VarEntry { v_ty=Public(ByteArr x) } ->
         ignore(unify_equal_lt (Public Int) (tc_expr venv expr) p)
       | VarEntry { v_ty=Private(ByteArr x) } ->
         ignore(unify_equal_lt (Private Int) (tc_expr venv expr) p)
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
       bytearr[0] = Private 1 -- right now we have to declare 1, but this
       seems like a nicer syntax
    *)
    ignore(unify_lt (tc_expr venv cond) (Public Bool) p);
    ignore(tc_stms fn_ty venv then');
    ignore(tc_stms fn_ty venv else');
  | For(name,l,h,body,p) ->
    (* TODO: Same as if statements *)
    ignore(unify_lt (tc_expr venv (Primitive(l,None))) (Public Int) p);
    ignore(unify_lt (tc_expr venv (Primitive(h,None))) (Public Int) p);
    (match (l,h) with
     | (Number l', Number h') ->
       if l' >= h' then raise
           (ForError ("Low value must be smaller than high value in for loop @ "
                      ^ (pos_string p)))
     | _ ->
       raise (TypeError ("Low and high values must be integers in for loop @ "
                         ^ (pos_string p))));
    let _ = Hashtbl.add venv name (LoopEntry { v_ty=(Public Int) }) in
    tc_stms fn_ty venv body
  | Return(expr,p) ->
    ignore(unify_lt fn_ty (tc_expr venv expr) p)

and tc_stms fn_ty venv stms =
  ignore(List.map (tc_stm fn_ty venv) stms)

and tc_fdec venv = function
  | FunctionDec(_,_,ByteArr(_),_,p) ->
    raise (TypeError("Functions cannot return a ByteArray @ " ^ (pos_string p)))
  | FunctionDec(name,args,ty,body,_) ->
    let venv' = Hashtbl.copy venv in
    let args_ty = List.map (fun { name=n; ty=t } ->
                              Hashtbl.add venv' n (VarEntry {v_ty=t}); t)
                    args in
    let _ = tc_stms (Public ty) venv' body in
    Hashtbl.add venv name (FunEntry { f_ty=ty; f_args=args_ty })

and tc_module (CModule l) =
  List.fold_left (fun a f -> ignore(tc_fdec Env.venv f)) () l
