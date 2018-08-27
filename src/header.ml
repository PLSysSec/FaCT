open Util
open Pos
open Err
open Tast

let sprintf = Printf.sprintf
let concat = String.concat

class headerator (m : fact_module) =
  object (visit)
    val mutable _indent : int = -1

    val _minfo : module_info =
      let Module(_,_,minfo) = m in minfo

    method _prindent n =
      "\n" ^ (String.make ((_indent + n) * 2) ' ')

    method fact_module () =
      let Module(sdecs,fdecs,_) = m in
      let sdecs' = (concat "\n\n" @@ List.map visit#sdec sdecs) in
      let fdecs' = (concat "\n\n" @@ List.map visit#fdec fdecs) in
        sdecs' ^ "\n\n" ^ fdecs'

    method sdec =
      xwrap @@ fun p ->
      fun (StructDef (name,fields)) ->
        sprintf
          "struct %s;
// struct %s {
%s
// };"
          name.data
          name.data
          (concat "\n" @@ List.map visit#field fields)

    method field =
      xwrap @@ fun p ->
      fun (Field (x,bty)) ->
        sprintf
          "//   %s %s;"
          (visit#bty bty)
          x.data

    method fdec =
      xwrap @@ fun p -> function
        | FunDec(fn,ft,rt,params,body) ->
            if not ft.export then
              sprintf "/* %s is an internal function */" fn.data
            else if ft.everhi then
              sprintf "/* %s is not an exportable function */" fn.data
            else
              let params' = concat "," @@ List.map visit#param params in
                sprintf "%s %s(%s);"
                  (visit#rty rt)
                  fn.data
                  params'
        | _ -> ""

    method rty = function
      | None -> "void"
      | Some bt -> visit#bty bt

    method param =
      xwrap @@ fun p -> function
        | Param (x,bty) ->
          sprintf "\n  %s %s%s"
            (visit#bty bty)
            x.data
            (visit#abty bty)

    method lbl =
      xwrap @@ fun p -> function
        | Public -> "/*public*/"
        | Secret -> "/*secret*/"

    method mut =
      xwrap @@ fun p -> function
        | R -> ""
        | W -> " *"
        | RW -> " *"

    method amut =
      xwrap @@ fun p -> function
        | R -> "const "
        | W -> ""
        | RW -> ""

    method bty =
      xwrap @@ fun p -> function
        | Bool l -> sprintf "%s uint8_t" (visit#lbl l)
        | UInt (s,l) -> sprintf "%s uint%d_t" (visit#lbl l) s
        | Int (s,l) -> sprintf "%s int%d_t" (visit#lbl l) s
        | Ref ({data=Struct s},m) -> sprintf "%sstruct %s *" (visit#amut m) s.data
        | Ref (bt,m) -> sprintf "%s%s" (visit#bty bt) (visit#mut m)
        | Arr ({data=Ref (bt,m)},lexpr,_) -> sprintf "%s%s" (visit#amut m) (visit#bty bt)
        | _ -> "X[bty]X"

    method abty =
      xwrap @@ fun p -> function
        | Arr (_,lexpr,_) -> sprintf "[%s]" (visit#lexpr lexpr)
        | _ -> ""

    method lexpr =
      xwrap @@ fun p -> function
        | LIntLiteral n -> string_of_int n
        | LDynamic x -> ""

  end

let generate_header m =
  let visit = new headerator m in
    visit#fact_module ()
