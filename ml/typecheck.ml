open Ast
open Types


(*

  This is a basic Hindley-Milner type checker. Right now it is able to type
  check any expr, but does not implement the return statement. I am not very
  fond of it and was unsure how it would type check so i excluded it.

  This works by unifying types from the bottom up. This means if the type
  checker does not know a type, it type checks a level lower until it finds the
  type and passes it back up. There is also a variable environment(venv). This
  stores the name of variables and functions and their corresponding types. For
  functions, it also stores the types of the arguments.

  A shortcoming of this typechecker is that arguments must be given an explicit
  type. This kinda sucks, but it is good enough for now.

  By having a typechecker, we should easily be able to restrict the set of
  AST's which get passed to the codegen. This subset should always pass the
  LLVM IR typecheck, allowing us to use all of the types that intersect the
  AST and LLVM IR.

*)

exception NotImplemented
exception VariableNotDefined of string
exception TypeError of string
exception UnknownType of string
exception CallError of string

type ast_types = { ast : expr; ty : constantc_type }
type prim_types = { prim : primitive; ty : constantc_type }

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

let rec tc_prim = function
  | Number n -> { prim=Number n; ty=Int }
  | Bool b -> { prim=Bool b; ty=Bool }

and tc_unop = function
  | B_Not -> raise (UnknownType "Type is unknown for boolean operator:\t ~")

and tc_bop = function
  | Plus -> Int
  | Minus -> Int
  | GT -> raise (UnknownType "Type is unknown for boolean operator:\t >")
  | B_And -> raise (UnknownType "Type is unknown for boolean operator:\t &")
  | B_Or -> raise (UnknownType "Type is unknown for boolean operator:\t |")

and tc_dec = function
  | VarDec(name,body) ->
    let { ast=ast; ty=v_ty } = tc_expr body in
    let _ = Hashtbl.add venv name (VarEntry { ty=v_ty }) in
    ()
  | FunctionDec(name,args,body) ->
    List.map (fun { name=n; ty=t } -> Hashtbl.add venv n (VarEntry {ty=t})) args;
    let body_ty = (tc_expr body).ty in
    let args_ty = List.map (fun { name=n; ty=ty} -> ty) args in
    Hashtbl.add venv name (FunEntry { ty=body_ty; args=args_ty });
    ()

and tc_expr = function
  | Primitive p ->
    let { prim=p'; ty=t} = tc_prim p in
    { ast=Primitive p'; ty=t }
  | Variable v ->
    (try
       match (Hashtbl.find venv v) with
       | VarEntry { ty=v_ty } -> { ast=Variable v; ty=v_ty }
       | FunEntry _ -> raise NotImplemented
    with
      Not_found -> raise (VariableNotDefined("Variable not defined:\t" ^ v)))
  | BinOp(op,e,e1) ->
    let op_ty = tc_bop op in
    let { ast=e'; ty=e_ty } = tc_expr e in
    let { ast=e1'; ty=e1_ty } = tc_expr e1 in
    let unified = unify op_ty (unify e_ty e1_ty) in
    { ast=BinOp(op,e',e1'); ty=unified }
  | UnaryOp(op,e) ->
    let op_ty = tc_unop op in
    let { ast=e'; ty=e_ty } = tc_expr e in
    { ast=UnaryOp(op,e'); ty=(unify e_ty op_ty) }
  | If(c,t,e) ->
    let { ast=c'; ty=c_ty } = tc_expr c in
    let { ast=t'; ty=t_ty } = tc_expr t in
    let { ast=e'; ty=e_ty } = tc_expr e in
    unify c_ty Bool;
    { ast=If(c',t',e'); ty=(unify t_ty e_ty) }
  | Seq(e,e1) -> tc_expr e; tc_expr e1;
  | Mutate _ -> raise NotImplemented
  | Dec d -> tc_dec d; { ast=Dec d; ty=Null }
  | CallExp(name,args) ->
    (try
      match Hashtbl.find venv name with
      | VarEntry _ -> raise (CallError ("Unable to call variable `" ^ name ^ "`"))
      | FunEntry { ty=ty; args=args' } ->
        List.map2 (fun carg farg -> unify (tc_expr carg).ty farg) args args';
        { ast=CallExp(name,args); ty=ty }
    with
      Not_found -> raise NotImplemented)
