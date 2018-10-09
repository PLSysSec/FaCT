open Util
open Pos
open Err
open Tast
open Tast_util

let sprintf = Printf.sprintf
let concat = String.concat

class ctverifator (m : fact_module) =
  object (visit)
    val mutable _indent : int = -1

    val _minfo : module_info =
      let Module(_,_,minfo) = m in minfo

    method _prindent n =
      "\n" ^ (String.make ((_indent + n) * 2) ' ')

    method fact_module this_include =
      let includes = concat "\n" ["#include <stdlib.h>"; "#include <stdint.h>"; "#include \"ctverif.h\""; this_include] in
      let Module(sdecs,fdecs,_) = m in
      let sdecs' = (concat "\n\n" @@ List.map visit#sdec sdecs) in
      let fdecs' = (concat "\n\n" @@ List.map visit#fdec fdecs) in
        sprintf "%s\n%s\n%s" includes sdecs' fdecs'

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
              let body' = concat "" @@ List.map visit#annotations params in
              let param_names = concat "," @@ List.map visit#param_name params in
                sprintf "%s %s_wrapper(%s) { \n\n%s\n%s%s(%s);\n}"
                  (visit#rty rt)
                  fn.data
                  params'
                  body'
                  (visit#rca rt) 
                  fn.data
                  param_names
        | _ -> ""

    method rty = function
      | None -> "void"
      | Some bt -> visit#bty bt

    method rca = function
      | None -> "    "
      | Some bt -> "    return "

    method annotations =
      xwrap @@ fun p -> function
        | Param (x,bty) ->
          (* For every public input value x, generate "public_in(__SMACK_value(x));" *)
          let is_public_label = match (label_of bty).data with
            | Public -> true 
            | Secret -> false in

          (* For every input pointer ptr, generate "public_in(__SMACK_value(ptr));" *)
          let is_ptr = ((is_ref bty) || (is_arr bty)) in

          (* Generate "public_in(__SMACK_value(X));" *)
          let s_val = match (is_public_label || is_ptr) with
            | true -> "    public_in(__SMACK_value(" ^ x.data ^ ")); \n"
            | false -> "" in

          (* For every public input array arr of constant size c, generate "public_in(__SMACK_values(arr,c));" *)
          let s_vals = match ((is_arr bty) && (is_public_label)) with
            | true -> "    public_in(__SMACK_values(" ^ x.data ^ "," ^ (visit#lexpr (length_of bty)) ^")); \n"
            | false -> "" in

          (* TODO: If the return value is public, generate "public_out(__SMACK_return_value());" (restrictive) *)
          (* TODO: declassify -- (permissive) takes a secret input; allows it to become a public output *)
          (* TODO: public_out -- (restrictive) takes a public input; ensures that a secret *never* flows to it (otherwise a secret may flow to a public var, as long as it doesn't affect timing) *)
          (* TODO: __disjoint_regions -- (permissive) takes two arrays and their lengths; assumes that they don't overlap (because e.g. if a secret arr and public arr overlap, then referencing the public arr could leak secrets) *)
          (* TODO: structs? *)

          let s = s_val ^ s_vals in
          sprintf "%s" s

    method param_name =
      xwrap @@ fun p -> function
        | Param (x,bty) -> sprintf "%s" x.data

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

let ctverify fname m =
  let this_include = concat "" ["#include \""; fname; ".h\""] in
  let visit = new ctverifator m in visit#fact_module this_include
