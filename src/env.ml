open Stdlib
open Ast

exception FunctionNotFound of string
exception NotImplemented

type entry =
  | VarEntry of Ast.labeled_type
  | FunEntry of { f_ty:Ast.expr_type; f_args:Ast.labeled_type list }

let varentry_lt (VarEntry pt) = { ty=pt.ty; label=pt.label }
let make_ref_entry lt = VarEntry { ty=lt.ty; label=lt.label; kind=Ref }

type env = (string,entry) Hashtbl.t

let venv =
  let v = Hashtbl.create 10 in
  let add_fun {name=n; ret_ty=ret; args_ty=args} =
    Hashtbl.add v n
      (FunEntry {f_ty={ ty=ret; label=None; kind=Val }; f_args=args}) in
  let _ = List.map add_fun stdlib_funs in
  v

let fun_ret = Hashtbl.create 10

let update_label name venv label =
  let update_label' = function
    | VarEntry { v_ty={ ty=t; label=l } } ->
      let lt = { ty=t; label=label; kind=Val } in
      let var_entry = VarEntry { v_ty=lt } in
      Hashtbl.replace venv name var_entry;
      lt
    | StaticVarEntry _ -> raise NotImplemented
    | LoopEntry { v_ty=lt } -> raise NotImplemented
    | FunEntry { f_ty=lt; f_args=args } -> raise NotImplemented in
  let var = (try Some(Hashtbl.find venv name) with | Not_found -> None) in
  match var with
    | None -> raise NotImplemented
    | Some entry -> update_label' entry

let save_fn_ret_label label f =
  Hashtbl.replace fun_ret f label

let get_fn_ret_label ~default f =
  try Hashtbl.find fun_ret f with
    | Not_found -> default

(* This defaults all VarEntrys to have a Secret label when it is ambiguous.
   It should be called after type checking a function, before rewriting it. *)
let default_to_secret venv =
  let update k v =
    match v with
      | VarEntry { v_ty={ ty=t; label=None } } ->
        ignore(update_label k venv (Some Secret))
      | _ -> () in
  Hashtbl.iter update venv

let print_env env =
  let print_env' k v =
    match v with
      | VarEntry { v_ty={ ty=_; label=None} } ->
        print_string(k ^ ": None\n")
      | VarEntry { v_ty={ ty=_; label=Some Public} } ->
        print_string(k ^ ": Public\n")
      | VarEntry { v_ty={ ty=_; label=Some Secret} } ->
        print_string(k ^ ": Secret\n")
      | _ -> () in
    Hashtbl.iter print_env' env
