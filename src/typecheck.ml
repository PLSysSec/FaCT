open Ast
open Env

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string

let unify t t1 p =
  match (t,t1) with
  | (Int,Int) -> Int
  | (Bool,Bool) -> Bool
  | (ByteArr x, ByteArr y) when x = y -> (ByteArr x)
  | _ -> raise (TypeError(ty_to_string(t) ^ " does not unify with " ^ ty_to_string(t1) ^ " @ " ^ (pos_string p)))

let unify_fn (rt,arg_ts) ts p =
  ignore(List.map2 (fun t t1 -> unify t t1 p) arg_ts ts);
  rt

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
  | Number n -> Int
  | ByteArray s -> ByteArr (List.length s)
  | Boolean b -> Bool

and tc_expr venv = function
  | VarExp(v,p) ->
    (try
       match Hashtbl.find venv v with
       | VarEntry { v_ty=ty } -> ty
       | LoopEntry _ -> Int
       | _ -> raise (VariableNotDefined(v))
     with
       Not_found -> raise (VariableNotDefined("Variable `" ^ v ^ "` not defined" ^ (pos_string p))))
  | ArrExp(v,i,p) ->
    (match i with
     | Primitive _ -> Int
     | VarExp(n,p) ->
       (try
          match Hashtbl.find venv n with
          | LoopEntry { v_ty=Int } -> Int
          | _ -> raise (TypeError ("Arrays can only be accessed with constant numbers or loop variables" ^ (pos_string p)))
       with
       | Not_found -> raise (VariableNotDefined("Variable `" ^ n ^ "` not defined @ " ^ (pos_string p)) ))
     | _ -> raise (TypeError ("Arrays can only be accessed with constant numbers or loop variables @ " ^ (pos_string p))))
  | UnOp(op,expr,p) ->
    let op_ty = tc_unop op in
    let expr_ty = tc_expr venv expr in
    unify_fn op_ty [expr_ty] p
  | BinOp(op,expr1,expr2,p) ->
    let op_ty = tc_binop op in
    let expr1_ty = tc_expr venv expr1 in
    let expr2_ty = tc_expr venv expr2 in
    unify_fn op_ty [expr1_ty;expr2_ty] p
  | Primitive(p,_) -> tc_prim p
  | CallExp(name,args,p) ->
    (try
       match Hashtbl.find venv name with
       | VarEntry _ ->
         raise (CallError ("Unable to call variable `" ^ name ^ "` @ " ^ (pos_string p)))
       | LoopEntry _ ->
         raise (CallError ("Unable to call loop variable `" ^ name ^ "` @ " ^ (pos_string p)))
       | FunEntry { f_ty=ty; f_args=args' } ->
         let fn_ty = (ty, args') in
         unify_fn fn_ty (List.map (tc_expr venv) args) p
     with
       Not_found -> raise NotImplemented)

and tc_stm fn_ty venv = function
  | VarDec(name,ty,expr,p) ->
    let expr_ty = tc_expr venv expr in
    Hashtbl.add venv name (VarEntry { v_ty=unify ty expr_ty p })
  | Assign(name,expr,p) ->
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (VariableNotDefined("Variable, `" ^ name ^ "`, not defined @ " ^ (pos_string p))) in
    (match v with
     | VarEntry { v_ty=ty } ->
       ignore(unify ty (tc_expr venv expr) p)
     | _ -> raise (VariableNotDefined("Variable, `" ^ name ^ "`, not defined @ " ^ (pos_string p))))
  | ArrAssign(name,index,expr,p) ->
    (try
      (match Hashtbl.find venv name with
       | VarEntry { v_ty=(ByteArr x) } ->
         ignore(unify Int (tc_expr venv expr) p)
       | _ -> raise (VariableNotDefined("Variable, `" ^ name ^ "`, not defined @ " ^ (pos_string p))))
    with
      Not_found -> raise (VariableNotDefined("Variable, `" ^ name ^ "`, not defined @ " ^ (pos_string p))))
  | If(cond,then',else',p) ->
    ignore(unify (tc_expr venv cond) Bool p);
    ignore(tc_stms fn_ty venv then');
    ignore(tc_stms fn_ty venv else')
  | For(name,l,h,body,p) ->
    ignore(unify (tc_expr venv (Primitive(l,None))) Int p);
    ignore(unify (tc_expr venv (Primitive(h,None))) Int p);
    (match (l,h) with
     | (Number l', Number h') ->
       if l' >= h' then raise
           (ForError ("Low value must be smaller than high value in for loop @ " ^ (pos_string p)))
     | _ ->
       raise (TypeError ("Low and high values must be integers in for loop @ " ^ (pos_string p))));
    let _ = Hashtbl.add venv name (LoopEntry { v_ty=Int }) in
    tc_stms fn_ty venv body
  | Return(expr,p) ->
    ignore(unify fn_ty (tc_expr venv expr) p)

and tc_stms fn_ty venv stms =
  ignore(List.map (tc_stm fn_ty venv) stms)

and tc_fdec venv = function
  | FunctionDec(_,_,ByteArr(_),_,p) ->
    raise (TypeError("Functions cannot return a ByteArray @ " ^ (pos_string p)))
  | FunctionDec(name,args,ty,body,_) ->
    let venv' = Hashtbl.copy venv in
    let args_ty = List.map (fun { name=n; ty=t } -> Hashtbl.add venv' n (VarEntry {v_ty=t}); t) args in
    let _ = tc_stms ty venv' body in
    Hashtbl.add venv name (FunEntry { f_ty=ty; f_args=args_ty })

and tc_module (CModule l) =
  List.fold_left (fun a f -> ignore(tc_fdec Env.venv f)) () l
