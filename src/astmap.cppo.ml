open Pos
open Err
open Tast

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

#define mkpos make_ast p @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function

type map_ctx_record = {
  ssenv : string Env.env;
}


class ast_visitor =
  object (visit)
    method fact_module m =
      let Module(fenv,fdecs,sdecs) = m in
      let fdecs' = List.map visit#fdec fdecs in
        Module(fenv,fdecs',sdecs)

    method fdec = pfunction
      | FunDec(f,ft,rt,params,body) ->
        let params' = List.map visit#param params in
        let body' = visit#block body in
          FunDec(f,ft,rt,params',body')
      | _ as f -> f

    method param = pfunction
      | Param(x,vty,attr) -> Param(x,vty,attr)

    method block (venv,stms) =
      (venv,visit#stms stms)

    method stm' = xfunction
      | BaseDec(x,vty,e) ->
        let e' = visit#expr e in
          [BaseDec(x,vty,e')]
      | ArrayDec(x,vty,ae) ->
        let ae' = visit#aexpr ae in
          [ArrayDec(x,vty,ae')]
      | StructDec _ as stm -> [stm]
      | Assign(lval,e) ->
        let lval' = visit#lval lval in
        let e' = visit#expr e in
          [Assign(lval', e')]
      | If(cond,tblock,fblock) ->
        let cond' = visit#expr cond in
        let tblock' = visit#block tblock in
        let fblock' = visit#block fblock in
          [If(cond',tblock',fblock')]
      | For(i,bty,init,cond,upd,block) ->
        let init' = visit#expr init in
        let cond' = visit#expr cond in
        let upd' = visit#expr upd in
        let block' = visit#block block in
          [For(i,bty,init',cond',upd',block')]
      | VoidFnCall(fname,args) ->
        let args' = List.map visit#arg args in
          [VoidFnCall(fname, args')]
      | DebugVoidFnCall _ as stm -> [stm]
      | Return e ->
        let e' = visit#expr e in
          [Return e']
      | VoidReturn as stm -> [stm]
      | Block block ->
        let block' = visit#block block in
          [Block block']

    method stm stm_ =
      let p = stm_.pos in
      let stms' = visit#stm' stm_ in
        List.map (fun s -> mkpos s) stms'

    method stms stms_ =
      List.flatten @@ List.map visit#stm stms_

    method aexpr {data=(ae,ety); pos=p} =
      let ae' =
        match ae with
          | ArrayLit es ->
            let es' = List.map visit#expr es in
              ArrayLit es'
          | ArrayVar lval ->
            let lval' = visit#lval lval in
              ArrayVar lval'
          | ArrayZeros lexpr -> ae
          | ArrayCopy lval ->
            let lval' = visit#lval lval in
              ArrayCopy lval'
          | ArrayView(lval,e,lexpr) ->
            let lval' = visit#lval lval in
            let e' = visit#expr e in
              ArrayView(lval', e', lexpr)
          | ArrayComp(bty,lexpr,x,e) ->
            let e' = visit#expr e in
              ArrayComp(bty, lexpr, x, e')
          | ArrayNoinit lexpr -> ae
          | CheckedArrayExpr(stms, subae) ->
            let stms' = visit#stms stms in
            let subae' = visit#aexpr subae in
              CheckedArrayExpr(stms', subae')
      in
        mkpos (ae',ety)

    method expr {data=(expr,ety); pos=p} : Tast.expr =
      let expr' =
        match expr with
          | Lvalue lval ->
            let lval' = visit#lval lval in
              Lvalue lval'
          | IntCast(bty,e) ->
            let e' = visit#expr e in
              IntCast(bty,e')
          | UnOp(op,e) ->
            let e' = visit#expr e in
              UnOp(op,e')
          | BinOp(op,e1,e2) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
              BinOp(op,e1',e2')
          | TernOp(e1,e2,e3) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
            let e3' = visit#expr e3 in
              TernOp(e1',e2',e3')
          | Select(e1,e2,e3) ->
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
            let e3' = visit#expr e3 in
              Select(e1',e2',e3')
          | FnCall(fname,args) ->
            let args' = List.map visit#arg args in
              FnCall(fname, args')
          | DebugFnCall _ -> expr
          | Declassify e ->
            let e' = visit#expr e in
              Declassify e'
          | Inject(x,stms) ->
            let stms' = visit#stms stms in
              Inject(x,stms')
          | Shuffle(e,mask) ->
            let e' = visit#expr e in
              Shuffle(e', mask)
          | _ -> expr
      in
        mkpos (expr',ety)

    method lval {data=(lval,vty); pos=p} =
      let lval' =
        match lval with
          | Base x as data -> data
          | ArrayEl(lval,e) ->
            let lval' = visit#lval lval in
            let e' = visit#expr e in
              ArrayEl(lval',e')
          | StructEl(lval,field) ->
            let lval' = visit#lval lval in
              StructEl(lval',field)
          | CheckedLval(stms, sublval) ->
            let stms' = visit#stms stms in
            let sublval' = visit#lval sublval in
              CheckedLval(stms', sublval')
      in
        mkpos (lval',vty)

    method arg = pfunction
      | ByValue e ->
        let e' = visit#expr e in
          ByValue e'
      | ByRef lval ->
        let lval' = visit#lval lval in
          ByRef lval'
      | ByArray(ae,mut) ->
        let ae' = visit#aexpr ae in
          ByArray(ae', mut)
  end
