open Stdlib
open Ast
open Tast

exception VariableNotDefined of string
exception FunctionNotDefined of string
exception NotImplemented
exception TypeError of string

let errVarNotDefined v =
  VariableNotDefined("Variable `" ^ v ^ "` not defined")
let errFnNotDefined v =
  FunctionNotDefined("Function `" ^ v ^ "` not defined")
let errFoundNotVar v =
  TypeError("Cannot use `" ^ v ^ "` as variable")
let errFoundNotFn v =
  TypeError("Cannot use `" ^ v ^ "` as function")

type fentry = { f_rty:Ast.ctype; f_rlbl:Ast.label; f_args:Ast.labeled_type list }

type entry =
  | VarEntry of Ast.labeled_type ref
  | FunEntry of fentry

type env = (string,entry) Hashtbl.t

let venv =
  let v = Hashtbl.create 10 in
  let add_fun {name=n; ret_ty=ret; args_ty=args} =
    Hashtbl.add v n
      (FunEntry {f_rty=ret; f_rlbl=Unknown; f_args=args}) in
  let _ = List.map add_fun stdlib_funs in
  v

let fun_ret = Hashtbl.create 10

let get_var venv v =
  try
    match Hashtbl.find venv v with
      | VarEntry lt -> !lt
      | _ -> raise @@ errFoundNotVar v
  with
      Not_found -> raise @@ errVarNotDefined v

let get_fn venv f =
  try
    match Hashtbl.find venv f with
      | FunEntry fentry -> fentry
      | _ -> raise @@ errFoundNotVar f
  with
    Not_found -> raise @@ errFnNotDefined f

let update_label venv name label =
  let update_label' = function
    | VarEntry lt ->
      (match !lt.label,label with
        | Unknown, _ -> ignore(lt := { !lt with label=label })
        | a, b when a = b -> ()
        | _ -> raise NotImplemented)
    | FunEntry _ -> raise NotImplemented in
  try
    update_label' (Hashtbl.find venv name) with
      Not_found -> raise NotImplemented

let update_fn venv { pos=p; data=tfdec } =
  let args = List.map (fun { data={ lt } } -> lt) tfdec.t_params in
  Hashtbl.add venv tfdec.t_name (FunEntry { f_rty=tfdec.t_rty; f_rlbl=tfdec.t_rlbl; f_args=args })

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
