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

let add_var env v lt p =
  let vtbl = get_vtbl env in
    if Hashtbl.mem vtbl v then raise_error p RedefiningVar;
    Hashtbl.add vtbl v (ref lt)

let rec find_var env (p:Pos.pos) =
  let find_var' fn vtbl p v =
    try
      Hashtbl.find vtbl v
    with
        Not_found -> fn v
  in
    match env with
      | TopEnv vtbl ->
        find_var' (fun v -> raise_error p VariableNotDefined) vtbl p
      | SubEnv(vtbl,env') ->
        let fv = find_var env' p in
        find_var' fv vtbl p

let get_var env v p =
  let lt = find_var env p v in !lt

let get_arr venv v p =
  let lt = find_var venv v p in
    match !lt.kind with
      | Arr _ -> { !lt with kind=Ref }
      | _ -> raise_error p ArrayNotDefined

let update_label venv name label p =
  let lt = find_var venv p name in
    match !lt.label,label with
      | Unknown, _ -> ignore(lt := { !lt with label=label })
      | a, b when Ast.equal_label a b -> ()
      | _ -> raise_error p UpdateLabelError

let fill_vtbl_public venv =
  let vtbl = get_vtbl venv in
    Hashtbl.iter (fun v lt ->
                   if !lt.label = Unknown
                   then lt := { !lt with label=Public })
      vtbl

type fentry = { f_rvt:Ast.var_type; f_args:Ast.labeled_type list }
[@@deriving show]

type fenv = (string,fentry) Hashtbl.t [@printer pp_hashtbl]
[@@deriving show]

let new_fenv () = Hashtbl.create 10

let has_fn = Hashtbl.mem

let get_fn fenv f p =
  try
    Hashtbl.find fenv f
  with
    Not_found -> raise_error p FunctionNotDefined

let add_fn = Hashtbl.add
