open Util
open Pos
open Err
open Tast
open Tast_util

class sanitychecker post_transform m =
  object (visit)
    inherit Tastmap.tast_visitor m as super

    method fdec fdec =
      let fdec' = super#fdec fdec in
      let p = fdec'.pos in
        begin
          match fdec'.data with
            | FunDec(fn,ft,rt,params,body) ->
              (* check mutable public vs everhi? *)
              (* check void/return statement vs return type? *)
              if not (ends_with_ret body) then
                raise @@ err p
            | CExtern(fn,rt,params) -> ()
        end;
        fdec'

    method block_only (blk,next) =
      let blk' = super#block_only (blk,next) in
      let p = blk'.pos in
        begin
          match blk.data with
            | If (cond,thens,elses) ->
              let cty = type_of cond in
                if not (is_bool cty) then
                  raise @@ err p;
                if post_transform && (label_of cty).data = Secret then
                  raise @@ err p
            | _ -> ()
        end;
        blk'

    method stm stm =
      let stm' = super#stm stm in
      let p = stm'.pos in
        begin
          match stm'.data with
            | VarDec (x,bty,e) ->
              let e_bty = type_of e in
                if not (e_bty =: bty) then
                  raise @@ cerr p
                             "expected %s, got %s"
                             (show_base_type bty)
                             (show_base_type e_bty)
            | FnCall (x,bty,fn,args) -> ()
            | VoidFnCall (fn,args) -> ()
            | Assign (e1,e2) ->
              let e1_ty = type_of e1 in
              let e2_ty = type_of e2 in
                begin
                  match e1_ty.data with
                    | Ref (subty,{data=W|RW}) ->
                      if not (e2_ty =: subty) then
                        raise @@ err p
                    | _ -> raise @@ err p
                end
            | Assume e -> ()
        end;
        stm'

    method expr (e,bty) =
      let e',bty' = super#expr (e,bty) in
      let p = e'.pos in
        begin
          match e'.data with
            | Deref e ->
              let e_bty = type_of e in
                begin
                  match e_bty.data with
                    | Ref (subty,{data=R|RW}) ->
                      if not (subty =: bty) then
                        raise @@ err p
                    | _ -> raise @@ err p
                end
            | _ -> ()
        end;
        e',bty'

  end

let transform post_transform m =
  let visit = new sanitychecker post_transform m in
    visit#fact_module ()
