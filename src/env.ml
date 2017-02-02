open Stdlib
open Ast

exception FunctionNotFound of string
exception NotImplemented

type entry =
  | VarEntry of Ast.labeled_type ref
  | FunEntry of { f_ty:Ast.expr_type; f_args:Ast.labeled_type list }

type env = (string,entry) Hashtbl.t

let venv =
  let v = Hashtbl.create 10 in
  let add_fun {name=n; ret_ty=ret; args_ty=args} =
    Hashtbl.add v n
      (FunEntry {f_ty={ ty=ret; label=None }; f_args=args}) in
  let _ = List.map add_fun stdlib_funs in
  v

let fun_ret = Hashtbl.create 10

let update_label venv name label =
  let update_label' = function
    | VarEntry lt ->
      ignore(lt := { !lt with label=label })
    | FunEntry _ -> raise NotImplemented in
  let var = (try Some(Hashtbl.find venv name) with | Not_found -> None) in
  match var with
    | None -> raise NotImplemented
    | Some entry -> update_label' entry

let save_fn_ret_label label f =
  Hashtbl.replace fun_ret f label

let get_fn_ret_label ~default f =
  try Hashtbl.find fun_ret f with
    | Not_found -> default

(*let print_env env =
  let print_env' k v =
    match v with
      | VarEntry { v_ty={ ty=_; label=None} } ->
        print_string(k ^ ": None\n")
      | VarEntry { v_ty={ ty=_; label=Some Public} } ->
        print_string(k ^ ": Public\n")
      | VarEntry { v_ty={ ty=_; label=Some Secret} } ->
        print_string(k ^ ": Secret\n")
      | _ -> () in
    Hashtbl.iter print_env' env*)
