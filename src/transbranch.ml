open Util
open Pos
open Err
open Tast
open Tast_util
open Transmap

let mfresh =
  let ctr = ref 0 in
  let mfresh' () =
    ctr := !ctr + 1;
    "__m" ^ (string_of_int !ctr)
  in
    mfresh'

class transbranch m =
  object (visit)
    inherit Transmap.transmap m as super
    val _branchflow : expr stack = Stack.create ()

    method _form_ctx () =
      Stack.fold sband (fake_pos@>True,sbool) _branchflow

    method block (blk_,next_) =
      let p = blk_.pos in
        match blk_.data with
          | If (cond,thens,elses) ->
            if (label_of (type_of cond)).data = Secret then
              let cond' = visit#expr cond in
              let mname = p@>(mfresh ()) in
              let mvar = (p@>Variable mname, sbool) in
                push mvar _branchflow;
                let thens' = visit#block thens in
                  drop _branchflow;
                  let mpos = p@>VarDec (mname, sbool, cond') in
                  let ncond = (p@>UnOp(Ast.LogicalNot,mvar), type_of mvar) in
                  let nmname = p@>(mfresh ()) in
                  let nmvar = (p@>Variable nmname, sbool) in
                    push nmvar _branchflow;
                    let elses' = visit#block elses in
                      drop _branchflow;
                      let mneg = p@>VarDec (nmname, sbool, ncond) in
                      let elseblock = (p@>Scope elses', visit#next next_) in
                      let p2 = (p@>ListOfStuff [mneg], p@>Block elseblock) in
                      let thenblock = (p@>Scope thens', p@>Block p2) in
                      let p1 = (p@>ListOfStuff [mpos], p@>Block thenblock) in
                        p1
            else (visit#block_only (blk_,next_), visit#next next_)
          | _ -> (visit#block_only (blk_,next_), visit#next next_)

    method stm stm_ =
      let p = stm_.pos in
      match stm_.data with
        | Assign (e1,e2) ->
          if not (Stack.is_empty _branchflow) then
            let e1' = visit#expr e1 in
            let e2' = visit#expr e2 in
            let {data=Ref (e1_ty,_)} = type_of e1' in
            let deref = (p@>Deref e1', e1_ty) in
            let select = (p@>Select (visit#_form_ctx (), e2', deref), e1_ty) in
              p@>Assign (e1', select)
          else
            super#stm stm_
        | _ -> super#stm stm_

    method expr (e_,bty_) =
      let p = e_.pos in
        match e_.data with
          | Variable {data=name}
            when name = Transmap.ctx ->
            visit#_form_ctx ()
          | BinOp ((LogicalAnd|LogicalOr) as op,e1,e2) ->
            if (label_of (type_of e1)).data = Secret then
              match op with
                | LogicalAnd ->
                  super#expr (p@>Select (e1, e2, (p@>False,sbool)), bty_)
                | LogicalOr ->
                  super#expr (p@>Select (e1, (p@>True,sbool), e2), bty_)
            else super#expr (e_,bty_)
          | TernOp (cond,e1,e2) ->
            if (label_of (type_of cond)).data = Secret then
              super#expr (p@>Select (cond, e1, e2), bty_)
            else super#expr (e_,bty_)
          | _ -> super#expr (e_,bty_)

  end

let transform m =
  let visit = new transbranch m in
    visit#fact_module ()
