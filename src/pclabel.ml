open Util
open Pos
open Err
open Tast
open Tast_util

type 'a stack = 'a Stack.t
let push = Stack.push
let pop = Stack.pop
let top = Stack.top

class pclabeler m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val _pc : label stack =
      let pc = Stack.create () in
        push (fake_pos@>Public) pc; pc
    val mutable _rp : label ref = ref (fake_pos@>Public)
    val mutable everhis : fun_name' list = []

    method fdec fdec_ =
      _rp <- ref (fake_pos@>Public);
      let fdec' = super#fdec fdec_ in
        match fdec'.data with
          | FunDec (fn,fnty,rt,params,body) ->
            let fnty' =
              if List.mem fn.data everhis then
                { fnty with everhi = true }
              else fnty in
              fdec'.pos @> FunDec (fn,fnty',rt,params,body)
          | CExtern (fn,rt,params) ->
            if List.mem fn.data everhis then
              raise @@ cerr fdec'.pos "calling function '%s' from secret control flow" fn.data
            else fdec'

    method stm (stm_,lbl_) =
      let old_rp = !_rp in
        begin
          match stm_.data with
            | If (cond,_,_) ->
              let bty = type_of cond in
              let lbl = label_of bty in
                push (top _pc +$ lbl) _pc
            | _ -> ()
        end;
        let (stm',_) = super#stm (stm_,lbl_) in
        let stm' =
          match stm_.data with
            | If _ ->
              pop _pc |> ignore;
              stm'
            | FnCall (_,_,fn,_)
            | VoidFnCall (fn,_) ->
              if (top _pc +$ !_rp).data = Secret then
                everhis <- fn.data :: everhis;
              stm'
            | RangeFor (_,_,_,_,blk)
            | ArrayFor (_,_,_,blk) ->
              if old_rp.data = Public && !_rp.data = Secret then
                (* re-run the entire block, but now with secret rp *)
                let (stm',_) = super#stm (stm_,lbl_) in
                  stm'
              else stm'
            | Return _
            | VoidReturn ->
              _rp := !_rp +$ top _pc;
              stm'
            | _ -> stm'
        in
          (stm',
           if List.mem _cur_fn.data everhis
           then fake_pos @> Secret
           else top _pc +$ !_rp)

  end

let transform m =
  let visit = new pclabeler m in
    visit#fact_module ()
