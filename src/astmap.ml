open Pos
open Err
open Ast

class ast_visitor =
  object (visit)

    method fact_module m =
      let Module(sdecs,fdecs) = m in
      let fdecs' = List.map visit#fdec fdecs in
        Module(sdecs,fdecs')

    method fdec =
      wrap @@ fun p -> function
        | FunDec(fn,ft,rt,params,body) ->
          let params' = List.map visit#param params in
          let body' = visit#block body in
            FunDec(fn,ft,rt,params',body')
        | _ as f -> f

    method param =
      wrap @@ fun p -> function
        | Param(x,bty) -> Param(x,bty)

    method block stms =
      visit#stms stms

    method stms stms_ =
      List.flatten @@ List.map visit#stm stms_

    method stm' =
      xwrap @@ fun p -> function
        | Block blk -> [Block (visit#block blk)]
        | VarDec (x,bty,e) ->
          let e' = visit#expr e in
            [VarDec (x,bty,e')]
        | FnCall (x,bty,fn,args) ->
          let args' = List.map visit#expr args in
            [FnCall (x,bty,fn,args')]
        | VoidFnCall (fn,args) ->
          let args' = List.map visit#expr args in
            [VoidFnCall (fn,args')]
        | Assign (e1,e2) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
            [Assign (e1',e2')]
        | If (cond,thens,elses) ->
          let cond' = visit#expr cond in
          let thens' = visit#block thens in
          let elses' = visit#block elses in
            [If (cond',thens',elses')]
        | RangeFor (x,bty,e1,e2,blk) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
          let blk' = visit#block blk in
            [RangeFor (x,bty,e1',e2',blk')]
        | ArrayFor (x,bty,e,blk) ->
          let e' = visit#expr e in
          let blk' = visit#block blk in
            [ArrayFor (x,bty,e',blk')]
        | Return e ->
          let e' = visit#expr e in
            [Return e']
        | VoidReturn -> [VoidReturn]

    method stm stm_ =
      let p = stm_.pos in
      let stms' = visit#stm' stm_ in
        List.map (fun s -> (make_ast p s)) stms'

    method expr e_ =
      let p = e_.pos in
      let e_' = make_ast p @@
        match e_.data with
          | True
          | False
          | UntypedIntLiteral _
          | IntLiteral (_,_)
          | Variable _ -> e_.data
          | ArrayLen _
          | Cast (_,_) -> raise @@ err p
          | UnOp (op,e) ->
            let e' = visit#expr e in
              UnOp (op,e')
          | BinOp (op,e1,e2) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
              BinOp (op,e1',e2')
          | TernOp (_,_,_)
          | Select (_,_,_)
          | Declassify _
          | Assume _
          | Enref _
          | Deref _
          | ArrayGet (_,_)
          | ArrayLit _
          | ArrayZeros _
          | ArrayCopy _
          | ArrayView (_,_,_)
          | Shuffle (_,_)
          | StructLit _
          | StructGet (_,_)
          | StringLiteral _ -> raise @@ err p
      in
        visit#expr_post e_'
    method expr_post e = e

  end
