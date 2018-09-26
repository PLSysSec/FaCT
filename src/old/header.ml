open Pos
open Err
open Tast

(* x for 'eXtract' *)
let xwrap f pa = f pa.pos pa.data

let gh_bty =
  xwrap @@ fun p -> function
    | UInt n -> Printf.sprintf "uint%d_t" n
    | Int  n -> Printf.sprintf  "int%d_t" n
    | Bool   -> Printf.sprintf  "uint8_t" (* XXX *)
    | Num  _ -> raise @@ err(p)

let gh_label' p = function
  | Public -> "/*public*/"
  | Secret -> "/*secret*/"
  | Unknown -> raise @@ err(p)

let gh_label =
  xwrap @@ fun p -> function
    | Fixed l -> gh_label' p l
    | Guess _ -> raise @@ err(p)

let gh_mut =
  xwrap @@ fun p -> function
    | Const -> ""
    | Mut -> " *"

let gh_amut =
  xwrap @@ fun p -> function
    | Const -> "const "
    | Mut -> ""

let gh_lexpr =
  xwrap @@ fun p -> function
    | LIntLiteral n -> string_of_int n
    | LDynamic _ -> ""

let gh_aty =
  xwrap @@ fun p -> function
    | ArrayAT(b,_) -> Printf.sprintf "%s" (gh_bty b)

let gh_aty_post =
  xwrap @@ fun p -> function
    | ArrayAT(_,lexpr) -> Printf.sprintf "[%s]" (gh_lexpr lexpr)

let gh_ety =
  xwrap @@ fun p -> function
    | BaseET(b,l) -> String.concat "\n" [gh_label l; gh_bty b]
    | ArrayET _ -> raise @@ err(p)

let gh_vty x =
  xwrap @@ fun p -> function
    | RefVT(b,l,m) -> Printf.sprintf "%s %s%s %s" (gh_label l) (gh_bty b) (gh_mut m) x
    | ArrayVT(a,l,m,_) -> Printf.sprintf "%s %s%s %s%s" (gh_label l) (gh_amut m) (gh_aty a) x (gh_aty_post a)
    | StructVT(s,m) -> Printf.sprintf "struct %s * %s%s" s.data (gh_amut m) x

let gh_rty = function
  | None -> "void"
  | Some ety -> gh_ety ety

let gh_param { data=Param(x,vty,_) } =
  "\n  " ^ gh_vty x.data vty

let gh_fdec fenv =
  xwrap @@ fun p -> function
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

let gh_field =
  xwrap @@ fun p -> function
    | Field(x,vty,is_pointer) ->
      match vty.data with
        | RefVT(b,l,_) ->
          Printf.sprintf
            "//   %s %s %s%s;"
            (gh_label l)
            (gh_bty b)
            (if is_pointer then "*" else "")
            x.data
        | ArrayVT(a,l,_,_) ->
          Printf.sprintf
            "//   %s %s%s %s%s;"
            (gh_label l)
            (gh_aty a)
            (if is_pointer then " *" else "")
            x.data
            (if is_pointer then "" else gh_aty_post a)
        | StructVT(s,_) ->
          Printf.sprintf
            "//   struct %s %s%s;"
            s.data
            (if is_pointer then "*" else "")
            x.data

let gh_sdec =
  xwrap @@ fun p -> function
    | Struct(sname,fields) ->
      Printf.sprintf
        "struct %s;
// struct %s {
%s
// };"
        sname.data
        sname.data
        (String.concat "\n" @@ List.map gh_field fields)


let gh_module (Module(fenv,fdecs,sdecs)) =
  String.concat "\n\n" @@ (List.map gh_sdec sdecs @ List.map (gh_fdec fenv) fdecs)

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
