open Stdlib
open Ast

exception FunctionNotFound of string
exception NotImplemented

type ventry = { v_ty: Ast.labeled_type }
type fentry = { f_ty: Ast.labeled_type; f_args: Ast.labeled_type list }

type entry =
  | VarEntry of ventry
  | LoopEntry of ventry
  | FunEntry of fentry

type env = (string,entry) Hashtbl.t
type fun_ret_env = (string,Ast.labeled_type) Hashtbl.t

let venv =
  let v = Hashtbl.create 10 in
  let add_fun {name=n; ret_ty=ret; args_ty=args} =
    Hashtbl.add v n (FunEntry {f_ty={ ty=ret; label=None }; f_args=args}) in
  let _ = List.map add_fun stdlib_funs in
  v

let fun_ret = Hashtbl.create 10

let update_label name label =
  let update_label' = function
    | VarEntry { v_ty={ ty=t; label=l } } ->
      let lt = { ty=t; label=label } in
      let var_entry = VarEntry { v_ty=lt } in
      Hashtbl.replace venv name var_entry;
      lt
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

(* This defaults all VarEntrys to have a Private label when it is ambiguous.
   It should be called after type checking a function, before rewriting it. *)
let default_to_private () =
  let update k v =
    match v with
      | VarEntry { v_ty={ ty=t; label=None } } ->
        ignore(update_label k (Some Private))
      | _ -> () in
  Hashtbl.iter update venv