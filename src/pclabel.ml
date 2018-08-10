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

    method fdec fdec_ =
      _rp <- ref (fake_pos@>Public);
      super#fdec fdec_

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
          (stm',top _pc +$ !_rp)

  end

let transform m =
  let visit = new pclabeler m in
    visit#fact_module ()
