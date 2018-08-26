open Util
open Pos
open Err
open Tast

class tast_visitor (m : fact_module) =
  object (visit)
    val _minfo : module_info =
      let Module(_,_,minfo) = m in minfo
    val mutable _cur_fn : fun_name = fake_pos @> ""
    val mutable _pre_inject : simple_statement list = []
    val mutable _post_inject : simple_statement list = []

    method fact_module () =
      let Module(sdecs,fdecs,_) = m in
      let sdecs' = List.map visit#sdec sdecs in
      let fdecs' = List.map visit#fdec fdecs in
        Module(sdecs',fdecs',_minfo)

    method sdec =
      wrap @@ fun p ->
      fun (StructDef (name,fields)) ->
        let fields' = List.map visit#field fields in
          StructDef (name,fields')

    method field =
      wrap @@ fun p ->
      fun (Field (x,bty)) ->
        Field (x,bty)

    method fdec =
      wrap @@ fun p -> function
        | FunDec(fn,ft,rt,params,body) ->
          _cur_fn <- fn;
          let params' = List.map visit#param params in
          let body' = visit#block body in
            FunDec(fn,ft,rt,params',body')
        | CExtern(fn,ft,rt,params) ->
          _cur_fn <- fn;
          let params' = List.map visit#param params in
            CExtern(fn,ft,rt,params')
        | StdLibFn(code,ft,rt,params) ->
          _cur_fn <- Stdlib.name_of code;
          let params' = List.map visit#param params in
            StdLibFn(code,ft,rt,params')

    method param param = param

    method block_only (blk_,next) =
      let p = blk_.pos in
      let blk_' =
        match blk_.data with
          | Scope (blk) ->
            let blk' = visit#block blk in
              Scope (blk')
          | ListOfStuff (stms) ->
            let stms' = List.map visit#stm stms in
              ListOfStuff (stms')
          | If (cond,thens,elses) ->
            let cond' = visit#expr cond in
            let thens' = visit#block thens in
            let elses' = visit#block elses in
              If (cond',thens',elses')
          | RangeFor (x,bty,e1,e2,blk) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
            let blk' = visit#block blk in
              RangeFor (x,bty,e1',e2',blk')
          | ArrayFor (x,bty,e,blk) ->
            let e' = visit#expr e in
            let blk' = visit#block blk in
              ArrayFor (x,bty,e',blk')
      in
        p@>blk_'

    method block (blk_,next_) =
      let blk' = visit#block_only (blk_,next_) in
      let next' = visit#next next_ in
      (blk',next')

    method next next_ =
      let p = next_.pos in
      let next' =
        match next_.data with
          | Block blk ->
            let blk' = visit#block blk in
              Block blk'
          | Return e ->
            let e' = visit#expr e in
              Return e'
          | VoidReturn -> VoidReturn
          | End -> End
      in
        p@>next'

    method stms stms_ =
      List.flatten @@ List.map visit#stm_wrapper stms_

    method stm stm_ =
      let p = stm_.pos in
      let stm_' =
        match stm_.data with
          | VarDec (x,bty,e) ->
            let e' = visit#expr e in
              VarDec (x,bty,e')
          | FnCall (x,bty,fn,args) ->
            let args' = List.map visit#expr args in
              FnCall (x,bty,fn,args')
          | VoidFnCall (fn,args) ->
            let args' = List.map visit#expr args in
              VoidFnCall (fn,args')
          | Assign (e1,e2) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
              Assign (e1',e2')
          | Cmov (e1,cond,e2) ->
            let e1' = visit#expr e1 in
            let cond' = visit#expr cond in
            let e2' = visit#expr e2 in
              Cmov (e1',cond',e2')
          | Assume e ->
            let e' = visit#expr e in
              Assume e'
      in
        p @> stm_'

    method stm_wrapper stm_ =
      let stm' = visit#stm stm_ in
      let stms' = visit#stm_post stm' in
      let stms' = _pre_inject @ stms' @ _post_inject in
        _pre_inject <- [];
        _post_inject <- [];
        stms'
    method stm_post stm = [stm]

    method expr (e_,bty_) =
      let p = e_.pos in
      let e_' =
        match e_.data with
          | True
          | False
          | IntLiteral _ -> e_.data
          | Variable x -> Variable x
          | Cast (bty,e) ->
            let e' = visit#expr e in
              Cast (bty,e')
          | UnOp (op,e) ->
            let e' = visit#expr e in
              UnOp (op,e')
          | BinOp (op,e1,e2) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
              BinOp (op,e1',e2')
          | TernOp (e1,e2,e3) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
            let e3' = visit#expr e3 in
              TernOp (e1',e2',e3')
          | Select (e1,e2,e3) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
            let e3' = visit#expr e3 in
              Select (e1',e2',e3')
          | Declassify e ->
            let e' = visit#expr e in
              Declassify e'
          | Classify e ->
            let e' = visit#expr e in
              Classify e'
          | Enref e ->
            Enref (visit#expr e)
          | Deref e ->
            let e' = visit#expr e in
              Deref e'
          | ArrayGet (e,lexpr) ->
            let e' = visit#expr e in
            let lexpr' = visit#lexpr lexpr in
              ArrayGet (e', lexpr')
          | ArrayLit es ->
            ArrayLit (List.map visit#expr es)
          | ArrayZeros lexpr ->
            ArrayZeros (visit#lexpr lexpr)
          | ArrayCopy e ->
            ArrayCopy (visit#expr e)
          | ArrayView (e,index,len) ->
            ArrayView (visit#expr e,
                       visit#lexpr index,
                       visit#lexpr len)
          | Shuffle (e,ns) ->
            Shuffle (visit#expr e, ns)
          | StructLit entries ->
            StructLit (List.map
                         (fun (field,e) ->
                            (field,visit#expr e))
                         entries)
          | StructGet (e,field) ->
            StructGet (visit#expr e,field)
          | StringLiteral _ -> raise @@ err p
      in
        visit#expr_post (e_.pos @> e_',bty_)
    method expr_post (e,bty) = (e,bty)

    method lexpr =
      wrap @@ fun p -> function
        | LIntLiteral _
        | LDynamic _ as lexpr -> lexpr

  end
