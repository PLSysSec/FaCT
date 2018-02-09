open Pos
open Err
open Tast

#define err(p) InternalCompilerError("from source" ^ __LOC__ << p)

(* x for 'eXtract' *)
let xwrap f pa = f pa.pos pa.data
#define xfunction xwrap @@ fun p -> function

let gh_bty = xfunction
  | UInt n -> Printf.sprintf "uint%d_t" n
  | Int  n -> Printf.sprintf  "int%d_t" n
  | Bool   -> Printf.sprintf  "uint8_t" (* XXX *)
  | Num  _ -> raise @@ err(p)

let gh_label' p = function
  | Public -> "/*public*/"
  | Secret -> "/*secret*/"
  | Unknown -> raise @@ err(p)

let gh_label = xfunction
  | Fixed l -> gh_label' p l
  | Guess _ -> raise @@ err(p)

let gh_mut = xfunction
  | Const -> ""
  | Mut -> "*"

let gh_amut = xfunction
  | Const -> "const "
  | Mut -> ""

let gh_lexpr = xfunction
  | LIntLiteral n -> string_of_int n
  | LDynamic _ -> ""

let gh_aty = xfunction
  | ArrayAT(b,_) -> Printf.sprintf "%s" (gh_bty b)

let gh_aty_post = xfunction
  | ArrayAT(_,lexpr) -> Printf.sprintf "[%s]" (gh_lexpr lexpr)

let gh_ety = xfunction
  | BaseET(b,l) -> String.concat "\n" [gh_label l; gh_bty b]
  | ArrayET _ -> raise @@ err(p)

let gh_vty x = xfunction
  | RefVT(b,l,m) -> Printf.sprintf "%s %s%s %s" (gh_label l) (gh_bty b) (gh_mut m) x
  | ArrayVT(a,l,m) -> Printf.sprintf "%s %s%s %s%s" (gh_label l) (gh_amut m) (gh_aty a) x (gh_aty_post a)

let gh_rty = function
  | None -> "void"
  | Some ety -> gh_ety ety

let gh_param { data=Param(x,vty) } =
  "\n  " ^ gh_vty x.data vty

let gh_fdec fenv = xfunction
  | FunDec(f,ft,rt,params,_) ->
    let _,everhi = Env.find_var fenv f in
    if not ft.export then
      Printf.sprintf "/* %s is an internal function */" f.data
    else if !everhi then
      Printf.sprintf "/* %s is not an exportable function */" f.data
    else
      let paramdecs = String.concat "," @@ List.map gh_param params in
        Printf.sprintf
          "%s %s(%s);"
          (gh_rty rt)
          f.data
          paramdecs
  | _ -> ""

let gh_module (Module(fenv,fdecs)) =
  String.concat "\n\n" @@ List.map (gh_fdec fenv) fdecs

let generate_header fname m =
  let header_name = fname
                    |> Filename.basename
                    |> String.uppercase_ascii in
    Printf.sprintf
"#ifndef __%s_H
#define __%s_H

%s

#endif /* __%s_H */"
      header_name
      header_name
      (gh_module m)
      header_name
