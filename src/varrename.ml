open Util
open Pos
open Err
open Ast
open Ast_util
open Astmap

type 'a mlist = 'a list ref

let mlist_push el alist =
  alist := el :: !alist

class var_renamer =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _vmap : (string * var_name) list = []
    val _vstack : (var_name * string) mlist stack = Stack.create ()
    val _vthisblock : string mlist stack = Stack.create ()

    method _newvar p x =
      if List.mem x.data !(top _vthisblock) then
        raise @@ cerr p
                   "redefinition of '%s'"
                   x.data
      else
      if Core.List.Assoc.mem !(top _vstack) x ~equal:vequal then
        warn @@ cerr p
                  "shadowing of '%s'"
                  x.data;
      let x' = make_fresh x.data in
        _vmap <- (x', x) :: _vmap ;
        mlist_push (x, x') (top _vstack) ;
        mlist_push x.data (top _vthisblock) ;
        { x with data=x' }

    method _getvar p x =
      match Core.List.Assoc.find !(top _vstack) x ~equal:vequal with
        | Some x' -> { x with data=x' }
        | None -> raise @@ cerr p
                             "variable not defined: '%s'"
                             x.data

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

    method stm_post =
      wrap @@ fun p -> function
        | VarDec (x,bty,e) ->
          let x' = visit#_newvar p x in
            VarDec (x',bty,e)
        | FnCall (x,bty,fn,args) ->
          let x' = visit#_newvar p x in
            FnCall (x',bty,fn,args)
        | RangeFor (x,bty,e1,e2,blk) ->
          let x' = visit#_newvar p x in
            RangeFor (x',bty,e1,e2,blk)
        | ArrayFor (x,bty,e,blk) ->
          let x' = visit#_newvar p x in
            ArrayFor (x',bty,e,blk)
        | stm -> stm

    method expr_post =
      wrap @@ fun p -> function
        | Variable x ->
          let x' = visit#_getvar p x in
            Variable x'
        | e -> e

  end

let transform m =
  let visit = new var_renamer in
    visit#fact_module m
