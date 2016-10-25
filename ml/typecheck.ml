open Ast


(*

  Type checker v2
  This is very similar to the one before except it uses statements.
  Every statement has the type 'NoneType', except Return statements. This kinda
  messes everything up so it might be worth reconsidering how Return is designed

  The type checker still unifies types. Unification is great because it
  works well with a real type system with type declarations. If we ever have
  type declarations, unify will need to be updated to address these needs.

*)

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string

type ventry = { ty: constantc_type }
type fentry = { ty: constantc_type; args: constantc_type list }

type entry =
  | VarEntry of ventry
  | FunEntry of fentry

let venv = Hashtbl.create 10

let unify t t1 =
  match (t,t1) with
  | (Int,Int) -> Int
  | (Bool,Bool) -> Bool
  | _ -> raise (TypeError(ty_to_string(t) ^ " does not unify with " ^ ty_to_string(t1)))


let rec tc_unop = function
  | B_Not -> Int
  | Negate -> Int

and tc_binop = function
  | Plus -> Int
  | Minus -> Int
  | GT -> Bool
  | B_And -> raise (UnknownType "Type is unknown for boolean operator:\t &")
  | B_Or -> raise (UnknownType "Type is unknown for boolean operator:\t |")

and tc_prim = function
  | Number n -> Int
  | ByteArray b -> ByteArr
  | Boolean b -> Bool

and tc_expr = function
  | VarExp v ->
    (try
       match Hashtbl.find venv v with
       | VarEntry { ty=ty } -> ty
       | _ -> raise (VariableNotDefined(v))
     with
       Not_found -> raise (VariableNotDefined("Variable not defined:\t" ^ v)))
  | Unop(op,expr) ->
    let op_ty = tc_unop op in
    let expr_ty = tc_expr expr in
    unify op_ty expr_ty
  | BinOp(op,expr1,expr2) ->
    let op_ty = tc_binop op in
    let expr1_ty = tc_expr expr1 in
    let expr2_ty = tc_expr expr2 in
    let unify_expr = unify expr1_ty expr2_ty in
    unify unify_expr op_ty
  | Primitive p -> tc_prim p
  | CallExp(name,args) ->
    (try
       match Hashtbl.find venv name with
       | VarEntry _ -> raise (CallError ("Unable to call variable `" ^ name ^ "`"))
       | FunEntry { ty=ty; args=args' } ->
         let _ = List.map2
                 (fun carg farg -> unify (tc_expr carg) farg)
                 args args' in
         ty
     with
       Not_found -> raise NotImplemented)

and tc_stm = function
  | VarDec(name,ty,body) ->
    let body_ty = tc_expr body in (* TODO: Should we just do type inference here? Or add and enforce an explicit type? *)
    let _ = Hashtbl.add venv name (VarEntry { ty=body_ty }) in
    NoneType
  | Assign(name,body) ->
    (match Hashtbl.find venv name with
     | VarEntry { ty=ty } ->
       let _ = unify ty (tc_expr body); in
       NoneType
    | _ -> raise (VariableNotDefined(name)))
  | If(cond,then',else') ->
    let _ = unify (tc_expr cond) Bool in
    let then_ty = tc_stms then' in
    let else_ty = tc_stms else' in
    let _ = unify then_ty else_ty; in
    NoneType
  | While(cond,body) ->
    let _ = unify (tc_expr cond) in
    let _ = tc_stms body in
    NoneType
  | Return(expr) -> tc_expr(expr)

and tc_stms = function
  | [] -> raise (TypeError "Cannot type check an empty statement")
  | [l] -> tc_stm l
  | (f::r) ->
    let _ = tc_stm f in
    tc_stms r

and tc_fdec = function
  | FunctionDec(name,args,ty,body) ->
    let _ = List.map (fun { name=n; ty=t } -> Hashtbl.add venv n (VarEntry {ty=t})) args in
    let body_ty = tc_stms body in
    let args_ty = List.map (fun { name=n; ty=ty} -> ty) args in
    Hashtbl.add venv name (FunEntry { ty=body_ty; args=args_ty });
    NoneType

and tc_module = function
  | FDec [] -> NoneType
  | FDec (f::r) ->
    let _ = tc_fdec f in
    tc_module(FDec(r))
