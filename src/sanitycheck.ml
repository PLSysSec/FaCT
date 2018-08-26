open Util
open Pos
open Err
open Tast
open Tast_util
open Pseudocode

class sanitychecker post_transform m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val _fmap : (fun_name * (ret_type * params)) list =
      let Module (_,fdecs,_) = m in
        List.map
          (function
            | {data=FunDec(fn,_,rt,params,_)
                  | CExtern(fn,_,rt,params)} -> (fn, (rt,params))
            | {data=StdlibFn(code,_,rt,params)} ->
              let fn = Stdlib.name_of code in
                (fn, (rt,params)))
          fdecs

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
            | CExtern(fn,ft,rt,params) -> ()
            | StdlibFn(fn,ft,rt,params) ->
              (* check mutable public vs everhi? *)
              (* check void/return statement vs return type? *)
              ()
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
                             (ps#bty bty)
                             (ps#bty e_bty)
            | FnCall (x,bty,fn,args) ->
              let rt,params = findfn _fmap fn in
                if (List.length args) <> (List.length params) then
                  raise @@ cerr p "arity mismatch when calling '%s'" fn.data;
                let _ = match rt with
                  | Some rty ->
                    if not (rty =: bty) then
                      raise @@ cerr p
                                 "expected %s, got %s"
                                 (ps#bty bty)
                                 (ps#bty rty)
                  | None ->
                    raise @@ cerr p
                               "expected %s, got void"
                               (ps#bty bty) in
                  ()
            | VoidFnCall (fn,args) ->
              let rt,params = findfn _fmap fn in
                if (List.length args) <> (List.length params) then
                  raise @@ cerr p "arity mismatch when calling '%s'" fn.data;
                let _ = match rt with
                  | Some rty ->
                    raise @@ cerr p
                               "expected void, got %s"
                               (ps#bty rty)
                  | None -> () in
                  ()
            | Assign (e1,e2) ->
              let e1_ty = type_of e1 in
              let e2_ty = type_of e2 in
                begin
                  match e1_ty.data with
                    | Ref (subty,{data=W|RW}) ->
                      if not (e2_ty =: subty) then
                        raise @@ cerr p
                                   "mismatch: %s vs %s"
                                   (ps#bty e2_ty)
                                   (ps#bty subty)
                    | _ -> raise @@ err p
                end
            | Cmov (e1,cond,e2) ->
              let e1_ty = type_of e1 in
              let cty = type_of cond in
                if not (is_bool cty) then
                  raise @@ err p;
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
            | Declassify e ->
              let e_bty = type_of e in
                if not (bty =: declassify e_bty) then
                  raise @@ err p
            | Classify e ->
              let e_bty = type_of e in
                if not (bty =: classify e_bty) then
                  raise @@ cerr p
                             "weird classify: %s vs %s"
                             (ps#bty bty)
                             (ps#bty (classify e_bty))
            | Deref e ->
              let e_bty = type_of e in
                begin
                  match e_bty.data with
                    | Ref (subty,{data=R|RW}) ->
                      if not (subty =: bty) then
                        raise @@ err p
                    | _ -> raise @@ err p
                end
            | Enref e ->
              let e_bty = type_of e in
                begin
                  match e_bty.data with
                    | Ref _
                    | Arr ({data=Ref _},_,_) ->
                      (* should this be just a warning instead? *)
                      raise @@ cerr p "creating a ref of a ref"
                    | _ -> ()
                end
            | _ -> ()
        end;
        e',bty'

  end

let transform post_transform m =
  let visit = new sanitychecker post_transform m in
    visit#fact_module ()
