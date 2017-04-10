open Stdlib
open Err
open Ast

let pp_hashtbl fmt vtbl =
  let pp = Format.pp_print_text fmt in
    begin
      pp "{ ";
      Hashtbl.iter
        (fun k v -> pp (k ^ "; "))
        vtbl;
      pp "}";
    end

type 'a envtbl = (string,'a ref) Hashtbl.t [@printer pp_hashtbl]

type 'a env =
  | TopEnv of 'a envtbl
  | SubEnv of 'a envtbl * 'a env

let pp_env fmt env =
  let pp = Format.pp_print_text fmt in
    begin
      pp "{ ";
      (match env with
        | TopEnv vtbl -> pp_hashtbl fmt vtbl
        | SubEnv(vtbl,_) -> pp_hashtbl fmt vtbl);
      pp "}";
    end

let new_env () = TopEnv (Hashtbl.create 10)

let sub_env env = SubEnv (Hashtbl.create 10, env)

let get_vtbl = function
  | TopEnv vtbl -> vtbl
  | SubEnv(vtbl,_) -> vtbl

let add_var env v des p =
  let vtbl = get_vtbl env in
    if Hashtbl.mem vtbl v then raise_error p (RedefiningVar v);
    Hashtbl.add vtbl v (ref des)

let rec find_var env (p:Pos.pos) =
  let find_var' fn vtbl p v =
    try
      Hashtbl.find vtbl v
    with
        Not_found -> fn v
  in
    match env with
      | TopEnv vtbl ->
        find_var' (fun v -> raise_error p (VariableNotDefined v)) vtbl p
      | SubEnv(vtbl,env') ->
        let fv = find_var env' p in
        find_var' fv vtbl p

(* TODO: Refactor these two functions. Also, is get_var even needed?? *)
let get_var env v (p:Pos.pos) =
  try !(find_var env p v) with
    | Not_found -> raise_error p (VariableNotDefined v)

let get_arr venv v (p:Pos.pos) =
  try !(find_var venv p v) with
    | Not_found -> raise_error p (ArrayNotDefined v)

(* Was `update_label` *)
let update_label venv name des p =
  let des' = find_var venv p name in
    match !des'.label,des.label with
      | Unknown, _ -> ignore(des' := { !des' with label=des.label })
      | a, b when Ast.equal_label a b -> ()
      | _ -> raise_error p UpdateLabelError

let fill_vtbl_public venv =
  let vtbl = get_vtbl venv in
    Hashtbl.iter (fun v des ->
                   if !des.label = Unknown
                   then des := { !des with label=Public })
      vtbl

type fentry = { f_rvt:Ast.description; f_args:Ast.description list }
[@@deriving show]

type fenv = (string,fentry) Hashtbl.t [@printer pp_hashtbl]
[@@deriving show]

let new_fenv () = Hashtbl.create 10

let has_fn = Hashtbl.mem

let get_fn fenv f p =
  try
    Hashtbl.find fenv f
  with
    Not_found -> raise_error p (FunctionNotDefined f)

let add_fn = Hashtbl.add
