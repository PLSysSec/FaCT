open Stdlib
open Err
open Ast

type fentry = { f_rvt:Ast.var_type; f_args:Ast.labeled_type list }
[@@deriving show]

type entry =
  | VarEntry of Ast.labeled_type ref
  | FunEntry of fentry
[@@deriving show]

type env = (string,entry) Hashtbl.t
let pp_env fmt venv = Format.pp_print_text fmt "venv"
let equal_env venv1 venv2 = true

let venv =
  let v = Hashtbl.create 10 in
  (*let add_fun {name=n; ret_ty=ret; args_ty=args} =
    Hashtbl.add v n
      (FunEntry {f_rvt=ret; f_rlbl=Unknown; f_args=args}) in
  let _ = List.map add_fun stdlib_funs in*)
  v

let add_var venv v lt =
  if Hashtbl.mem venv v then raise (UnclassifiedError "redefining var");
  Hashtbl.add venv v (VarEntry (ref lt))

let get_var venv v =
  try
    match Hashtbl.find venv v with
      | VarEntry lt -> !lt
      | _ -> raise @@ errFoundNotVar v
  with
      Not_found -> raise @@ errVarNotDefined v

let get_arr venv v =
  try
    match Hashtbl.find venv v with
      | VarEntry lt ->
        (match !lt.kind with
          | Arr _ -> { !lt with kind=Ref }
          | _ -> raise @@ errFoundNotArr v)
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
        | _ -> raise @@ UnclassifiedError (name ^ " already has label " ^ (show_label !lt.label) ^", cannot change to " ^ (show_label label)))
    | FunEntry _ -> raise NotImplemented in
  try
    update_label' (Hashtbl.find venv name) with
      Not_found -> raise NotImplemented

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
