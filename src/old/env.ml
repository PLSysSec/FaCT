open Pos
open Err

let pp_hashtbl fmt vtbl = ()

type 'a envtbl = (string,'a) Hashtbl.t [@printer pp_hashtbl]
[@@deriving show]

let pp_envtbl pp_v fmt vtbl =
  let pp = Format.pp_print_text fmt in
    begin
      pp "{ ";
      Hashtbl.iter
        (fun k v -> pp k; pp "; ")
        vtbl;
      pp "}";
    end

type 'a env =
  | TopEnv of 'a envtbl
  | SubEnv of 'a envtbl * 'a env
[@@deriving show]

let pp_env pp_v fmt env =
  let pp = Format.pp_print_text fmt in
    begin
      pp "{ ";
      (match env with
        | TopEnv vtbl -> pp_envtbl pp_v fmt vtbl
        | SubEnv(vtbl,_) -> pp_envtbl pp_v fmt vtbl);
      pp "}";
    end

let new_env () = TopEnv (Hashtbl.create 10)

let sub_env env = SubEnv (Hashtbl.create 10, env)

let get_vtbl = function
  | TopEnv vtbl -> vtbl
  | SubEnv(vtbl,_) -> vtbl

let rec map f = function
  | TopEnv vtbl ->
    let env' = new_env () in
    let vtbl' = get_vtbl env' in
      Hashtbl.iter (fun k v -> Hashtbl.add vtbl' k (f v)) vtbl;
      env'
  | SubEnv(vtbl,env) ->
    let env' = sub_env (map f env) in
    let vtbl' = get_vtbl env' in
      Hashtbl.iter (fun k v -> Hashtbl.add vtbl' k (f v)) vtbl;
      env'

let rec iter f = function
  | TopEnv vtbl ->
    Hashtbl.iter f vtbl
  | SubEnv(vtbl,env) ->
    iter f env;
    Hashtbl.iter f vtbl

let rec fold f env init =
  match env with
    | TopEnv vtbl ->
      Hashtbl.fold f vtbl init
    | SubEnv(vtbl,env) ->
      let init' = fold f env init in
        Hashtbl.fold f vtbl init'

let add_var env v lt =
  let vtbl = get_vtbl env in
    if Hashtbl.mem vtbl v.data then raise (errRedefVar v);
    Hashtbl.add vtbl v.data lt

let replace_var env v lt =
  let vtbl = get_vtbl env in
    Hashtbl.replace vtbl v.data lt

let add_raw env v lt =
  let vtbl = get_vtbl env in
    if Hashtbl.mem vtbl v then raise (InternalCompilerError "internal redefinition");
    Hashtbl.add vtbl v lt

let remove_var env v =
  let vtbl = get_vtbl env in
  Hashtbl.remove vtbl v.data

let rec find_var' fnyes fnno env =
  let find_var_helper fnno vtbl v =
    try
      fnyes @@ Hashtbl.find vtbl v.data
    with
        Not_found -> fnno v
  in
    match env with
      | TopEnv vtbl ->
        find_var_helper fnno vtbl
      | SubEnv(vtbl,env') ->
        find_var_helper (find_var' fnyes fnno env') vtbl

let find_var name = find_var' (fun v -> v) (fun v -> raise @@ errVarNotDefined v) name

let has_var name =
  find_var' (fun _ -> true) (fun _ -> false) name
