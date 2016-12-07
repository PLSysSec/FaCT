open Ast
open Env

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string

let unify t t1 =
  match (t,t1) with
  | (Int,Int) -> Int
  | (Bool,Bool) -> Bool
  | (ByteArr x, ByteArr y) when x = y -> (ByteArr x)
  | _ -> raise (TypeError(ty_to_string(t) ^ " does not unify with " ^ ty_to_string(t1)))

let unify_fn (rt,arg_ts) ts =
  let _ = List.map2 unify arg_ts ts in
  rt

let rec tc_unop = function
  | B_Not -> (Int, [Int])

and tc_binop = function
  | Plus -> (Int, [Int;Int])
  | Minus -> (Int, [Int;Int])
  | Multiply -> (Int, [Int;Int])
  | Equal -> (Bool, [Int;Int])
  | NEqual -> (Bool, [Int;Int])
  | GT -> (Bool, [Int;Int])
  | GTE -> (Bool, [Int;Int])
  | LT -> (Bool, [Int;Int])
  | LTE -> (Bool, [Int;Int])
  | B_And -> (Int, [Int;Int])
  | B_Or -> (Int, [Int;Int])
  | LeftShift -> (Int, [Int;Int])
  | RightShift -> (Int, [Int;Int])

and tc_prim = function
  | Number n -> Int
  | ByteArray s -> ByteArr (List.length s)
  | Boolean b -> Bool

and tc_expr venv = function
  | VarExp v ->
    (try
       match Hashtbl.find venv v with
       | VarEntry { v_ty=ty } -> ty
       | LoopEntry _ -> Int
       | _ -> raise (VariableNotDefined(v))
     with
       Not_found -> raise (VariableNotDefined("Variable not defined:\t" ^ v)))
  | ArrExp(v,i) ->
    (match i with
     | Primitive _ -> Int
     | VarExp n ->
       (try
          match Hashtbl.find venv n with
          | LoopEntry { v_ty=Int } -> Int
          | _ -> raise (TypeError "Arrays can only be accessed with constant numbers or loop variables")
       with
       | Not_found -> raise (VariableNotDefined("Variable not defined:\t" ^ n)))
     | _ -> raise (TypeError "Arrays can only be accessed with constant numbers or loop variables"))
  | Unop(op,expr) ->
    let op_ty = tc_unop op in
    let expr_ty = tc_expr venv expr in
    unify_fn op_ty [expr_ty]
  | BinOp(op,expr1,expr2) ->
    let op_ty = tc_binop op in
    let expr1_ty = tc_expr venv expr1 in
    let expr2_ty = tc_expr venv expr2 in
    unify_fn op_ty [expr1_ty;expr2_ty]
  | Primitive p -> tc_prim p
  | CallExp(name,args) ->
    (try
       match Hashtbl.find venv name with
       | VarEntry _ ->
         raise (CallError ("Unable to call variable `" ^ name ^ "`"))
       | LoopEntry _ ->
         raise (CallError ("Unable to call loop variable `" ^ name ^ "`"))
       | FunEntry { f_ty=ty; f_args=args' } ->
         let fn_ty = (ty, args') in
         unify_fn fn_ty (List.map (tc_expr venv) args)
     with
       Not_found -> raise NotImplemented)

and tc_stm fn_ty venv = function
  | VarDec(name,ty,expr) ->
    let expr_ty = tc_expr venv expr in
    Hashtbl.add venv name (VarEntry { v_ty=unify ty expr_ty })
  | Assign(name,expr) ->
    let v = try Hashtbl.find venv name with
      | Not_found -> raise (VariableNotDefined(name)) in
    (match v with
     | VarEntry { v_ty=ty } ->
       ignore(unify ty (tc_expr venv expr))
     | _ -> raise (VariableNotDefined(name)))
  | ArrAssign(name,index,expr) ->
    (try
      (match Hashtbl.find venv name with
       | VarEntry { v_ty=(ByteArr x) } ->
         ignore(unify Int (tc_expr venv expr))
       | _ -> raise (VariableNotDefined(name)))
    with
      Not_found -> raise (VariableNotDefined(name)))
  | If(cond,then',else') ->
    ignore(unify (tc_expr venv cond) Bool);
    ignore(tc_stms fn_ty venv then');
    ignore(tc_stms fn_ty venv else')
  | For(name,l,h,body) ->
    ignore(unify (tc_expr venv (Primitive l)) Int);
    ignore(unify (tc_expr venv (Primitive h)) Int);
    (match (l,h) with
     | (Number l', Number h') ->
       if l' >= h' then raise
           (ForError "Low value must be smaller than high value in for loop")
     | _ -> raise
              (TypeError "Low and high values must be integers in for loop"));
    let _ = Hashtbl.add venv name (LoopEntry { v_ty=Int }) in
    tc_stms fn_ty venv body
  | Return(expr) ->
    ignore(unify fn_ty (tc_expr venv expr))

and tc_stms fn_ty venv stms =
  ignore(List.map (tc_stm fn_ty venv) stms)

and tc_fdec venv = function
  | FunctionDec(_,_,ByteArr(_),_) ->
    raise (TypeError "Functions cannot return a ByteArray")
  | FunctionDec(name,args,ty,body) ->
    let venv' = Hashtbl.copy venv in
    let args_ty = List.map (fun { name=n; ty=t } -> Hashtbl.add venv' n (VarEntry {v_ty=t}); t) args in
    let _ = tc_stms ty venv' body in
    Hashtbl.add venv name (FunEntry { f_ty=ty; f_args=args_ty })

and tc_module (CModule l) =
  List.fold_left (fun a f -> ignore(tc_fdec Env.venv f)) () l
