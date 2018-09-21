open Util
open Pos
open Err
open Ast
open Ast_util
open Astmap

class var_renamer =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _vmap : (string * var_name) list = []
    val _vstack : (var_name * string) mlist stack = Stack.create ()
    val _vthisblock : string mlist stack = Stack.create ()
    val _preproc : (var_name * int) mlist = ref []

    method _newvar p x =
      if List.mem x.data !(top _vthisblock) then
        raise @@ cerr p
                   "redefinition of '%s'"
                   x.data
      else
      if Core.List.Assoc.mem !(top _vstack) x ~equal:vequal then
        warn @@ werr p
                  "shadowing of '%s'"
                  x.data;
      let x' = make_fresh x.data in
        _vmap <- (x', x) :: _vmap ;
        mlist_push (x, x') (top _vstack) ;
        mlist_push x.data (top _vthisblock) ;
        { x with data=x' }

    method _getvar p x =
      match mlist_find ~equal:vequal !_preproc x with
        | Some n -> UntypedIntLiteral n
        | None ->
          begin
            match mlist_find ~equal:vequal !(top _vstack) x with
              | Some x' -> Variable { x with data=x' }
              | None -> raise @@ cerr p
                                   "variable not defined: '%s'"
                                   x.data
          end

    method fdec fdec =
      push (ref []) _vstack ;
      push (ref []) _vthisblock ;
      let res = super#fdec fdec in
        pop _vthisblock |> ignore ;
        pop _vstack |> ignore ;
        res

    method param param_ =
      let param' = super#param param_ in
      let p = param'.pos in
        begin
          match param'.data with
            | Param (x,bty) ->
              let x' = visit#_newvar p x in
                Param (x',bty)
        end |> make_ast p

    method block blk =
      push (ref !(top _vstack)) _vstack ;
      push (ref []) _vthisblock ;
      let res = super#block blk in
        pop _vthisblock |> ignore ;
        pop _vstack |> ignore ;
        res

    method stm' ({pos=p;data} as stm_) =
      match data with
        | RangeFor (x,bty,e1,e2,blk) ->
          (* scoping for loop var *)
          push (ref !(top _vstack)) _vstack ;
          push (ref []) _vthisblock ;
          let x' = visit#_newvar p x in
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
          let blk' = visit#block blk in
          let res = [RangeFor (x',bty,e1',e2',blk')] in
            pop _vthisblock |> ignore ;
            pop _vstack |> ignore ;
            res
        | ArrayFor (x,bty,e,blk) ->
          (* scoping for loop var *)
          push (ref !(top _vstack)) _vstack ;
          push (ref []) _vthisblock ;
          let x' = visit#_newvar p x in
          let e' = visit#expr e in
          let blk' = visit#block blk in
          let res = [ArrayFor (x',bty,e',blk')] in
            pop _vthisblock |> ignore ;
            pop _vstack |> ignore ;
            res
        | BigFor (i,n1,n2,blk) ->
          let rec rep n acc =
            if n < n2 then
              begin
                mlist_push (i,n) _preproc;
                let blk' = visit#block blk in
                  mlist_drop _preproc;
                  rep (n + 1) (acc @ blk')
              end
            else acc in
          let res = rep n1 [] in
            List.map (fun {data} -> data) res
        | _ -> super#stm' stm_

    method stm_post =
      wrap @@ fun p -> function
        | VarDec (x,bty,e) ->
          let x' = visit#_newvar p x in
            VarDec (x',bty,e)
        | FnCall (x,bty,fn,args) ->
          let x' = visit#_newvar p x in
            FnCall (x',bty,fn,args)
        | stm -> stm

    method expr_post =
      wrap @@ fun p -> function
        | Variable x ->
          visit#_getvar p x
        | e -> e

  end

let transform m =
  let visit = new var_renamer in
    visit#fact_module m
