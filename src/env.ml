open Ast

type ventry = { v_ty: constantc_type }
type fentry = { f_ty: constantc_type; f_args: constantc_type list }

type entry =
  | VarEntry of ventry
  | FunEntry of fentry

(** Create the environment *)
let venv =
  let v = Hashtbl.create 10 in
  let _ = Hashtbl.add v "printf" (FunEntry { f_ty=Int; f_args=[ByteArr] }) in
  v
