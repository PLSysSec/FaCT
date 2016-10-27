open Ast
open Env
(*

  Type checker v2
  This is very similar to the one before except it uses statements.

  The type checker still unifies types. Unification is great because it
  works well with a real type system with type declarations. If we ever have
  type declarations, unify will need to be updated to address these needs.

*)

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string

let unify t t1 =
  match (t,t1) with
  | (Int,Int) -> Int
  | (Bool,Bool) -> Bool
  | (ByteArr, ByteArr) -> ByteArr
  | _ -> raise (TypeError(ty_to_string(t) ^ " does not unify with " ^ ty_to_string(t1)))

let unify_fn (rt,arg_ts) ts =
  let _ = List.map2 unify arg_ts ts in
  rt

let rec tc_unop = function
  | B_Not -> (Int, [Int])
  | Negate -> (Int, [Int])

and tc_binop = function
  | Plus -> (Int, [Int;Int])
  | Minus -> (Int, [Int;Int])
  | GT -> (Bool, [Int;Int])
  | B_And -> (Int, [Int;Int])
  | B_Or -> (Int, [Int;Int])

and tc_prim = function
  | Number n -> Int
  | ByteArray b -> ByteArr
  | Boolean b -> Bool

and tc_expr venv = function
  | VarExp v ->
    (try
       match Hashtbl.find venv v with
       | VarEntry { v_ty=ty } -> ty
       | _ -> raise (VariableNotDefined(v))
     with
       Not_found -> raise (VariableNotDefined("Variable not defined:\t" ^ v)))
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
       | VarEntry _ -> raise (CallError ("Unable to call variable `" ^ name ^ "`"))
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
    (match Hashtbl.find venv name with
     | VarEntry { v_ty=ty } ->
       let _ = unify ty (tc_expr venv expr) in ()
     | _ -> raise (VariableNotDefined(name)))
  | If(cond,then',else') ->
    let _ = unify (tc_expr venv cond) Bool in
    let _ = tc_stms fn_ty venv then' in
    let _ = tc_stms fn_ty venv else' in ()
  | While(cond,body) ->
    let _ = unify (tc_expr venv cond) Bool in
    let _ = tc_stms fn_ty venv body in ()
  | Return(expr) ->
    let _ = unify fn_ty (tc_expr venv expr) in ()

and tc_stms fn_ty venv stms =
  let _ = List.map (tc_stm fn_ty venv) stms in ()

and tc_fdec venv = function
  | FunctionDec(name,args,ty,body) ->
    let venv' = Hashtbl.copy venv in
    let args_ty = List.map (fun { name=n; ty=t } -> Hashtbl.add venv' n (VarEntry {v_ty=t}); t) args in
    let _ = tc_stms ty venv' body in
    Hashtbl.add venv name (FunEntry { f_ty=ty; f_args=args_ty })

and tc_module (FDec l) =
  List.fold_left (fun a f -> let _ = tc_fdec Env.venv f in ()) () l
