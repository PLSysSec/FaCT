open Util
open Pos
open Err
open Ast
open Ast_util
open Astmap

let make_fresh =
  let ctr = ref 0 in
  let make_fresh' name =
    ctr := !ctr + 1;
    "__v" ^ (string_of_int !ctr) ^ "_" ^ name
  in
    make_fresh'

type 'a mlist = 'a list ref
type 'a stack = 'a Stack.t

let mlist_push el alist =
  alist := el :: !alist

class var_renamer =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _vmap : (string * var_name) list = []
    val _vstack : (var_name * string) mlist stack = Stack.create ()
    val _vthisblock : string mlist stack = Stack.create ()

    method _newvar p x =
      if List.mem x.data !(Stack.top _vthisblock) then
        raise @@ cerr p
                   "redefinition of '%s'"
                   x.data
      else
        let x' = make_fresh x.data in
          _vmap <- (x', x) :: _vmap ;
          mlist_push (x, x') (Stack.top _vstack) ;
          mlist_push x.data (Stack.top _vthisblock) ;
          { x with data=x' }

    method _getvar p x =
      match Core.List.Assoc.find !(Stack.top _vstack) x ~equal:vequal with
        | Some x' -> { x with data=x' }
        | None -> raise @@ cerr p
                             "variable not defined: '%s'"
                             x.data

    method block blk =
      Stack.push (ref !(Stack.top _vstack)) _vstack ;
      Stack.push (ref []) _vthisblock ;
      let res = super#block blk in
        Stack.pop _vthisblock |> ignore ;
        Stack.pop _vstack |> ignore ;
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

    method fdec fdec =
      Stack.push (ref []) _vstack ;
      Stack.push (ref []) _vthisblock ;
      let res = super#fdec fdec in
        Stack.pop _vthisblock |> ignore ;
        Stack.pop _vstack |> ignore ;
        res

  end

let transform m =
  let visit = new var_renamer in
    visit#fact_module m
