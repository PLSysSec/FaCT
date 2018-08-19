open Util
open Pos
open Err
open Tast
open Tast_util
open Transmap

type tripstate = Untripped | Pending | Tripped

let srbool = fake_pos @> Ref (sbool, fake_pos @> RW)

let rctx = "__rctx"
let rval = "__rval"

class transret m =
  object (visit)
    inherit Transmap.transmap m as super
    val mutable _tripped : tripstate = Untripped

    method _is_everhi fn =
      let fdec = findfn _minfo.fmap fn in
        match fdec.data with
          | FunDec (_,{ everhi },_,_,_) -> everhi
          | _ -> false

    method fdec fdec =
      let p = fdec.pos in
        _tripped <- Untripped;
        let fdec' = super#fdec fdec in
          match fdec'.data with
            | FunDec (fn,fty,rt,params,blk) ->
              let btrue = (p@>True,sbool) in
              let reftrue = (p@>Enref btrue,srbool) in
              let rctx = p @> VarDec (p@>rctx,srbool,reftrue) in
              let pre,ret =
                match rt with
                  | Some bty ->
                    let zero =
                      (p@>(match bty.data with
                             | Bool _ -> False
                             | UInt _ | Int _ -> IntLiteral 0
                             | _ -> raise @@ err p)
                      ,bty)
                    in
                    let rbty = p@>Ref (bty, p@>RW) in
                    let rvalvar = p @> VarDec (p@>rval,rbty,zero) in
                    let ret = p @> Return (p@>Deref (p@>Variable (p@>rval),rbty),bty) in
                      p@>ListOfStuff [rvalvar;rctx], ret
                  | None ->
                    let ret = p @> VoidReturn in
                      p@>ListOfStuff [rctx], ret in
              let blk' =
                p@>Block (if _tripped = Tripped
                          then (replace_final_next blk ret)
                          else blk) in
              let blk' = (pre, blk') in
                p @> FunDec (fn,fty,rt,params,blk')
            | CExtern _ -> fdec'

    method block (blk_,next) =
      let p = blk_.pos in
      let newblk,next' =
        match blk_.data with
          | Scope _
          | If _
          | RangeFor _
          | ArrayFor _ ->
            let blk_' = visit#block_only (blk_, next) in
              if _tripped = Pending then
                _tripped <- Tripped;
              let next' =
                if _tripped = Tripped then
                  let rctx = (p@>Variable (p@>rctx),srbool) in
                  let rcond = (p@>Deref rctx,sbool) in
                  let nextthens = (p@>ListOfStuff [], next) in
                  let nextelses = (p@>ListOfStuff [], p@>End) in
                  let newnextthing =
                    if next.data = End then ListOfStuff []
                    else If (rcond,nextthens,nextelses) in
                    p@>Block (p@>newnextthing, p@>End)
                else next in
                (blk_',next')
          | _ ->
            visit#block_only (blk_,next), next
      in
      let newblk' =
        if _tripped = Tripped then
          let wrapblock blk =
            let rctx = (p@>Variable (p@>rctx),srbool) in
            let rcond = (p@>Deref rctx,sbool) in
            let emptyelses = (p@>ListOfStuff [], p@>End) in
              (p@>If (rcond,blk,emptyelses),p@>End)
          in
            match newblk.data with
              | RangeFor (x,bty,e1,e2,blk) ->
                p@>RangeFor (x,bty,e1,e2,wrapblock blk)
              | ArrayFor (x,bty,e,blk) ->
                p@>ArrayFor (x,bty,e,wrapblock blk)
              | _ -> newblk
        else newblk in
        (newblk', visit#next next')

    method next next_ =
      let p = next_.pos in
        match next_.data with
          | Return e ->
            let e' = visit#expr e in
              if not (Stack.is_empty _secretflow && _tripped = Untripped) then begin
                if _tripped = Untripped then
                  _tripped <- Pending;
                let fdec = findfn _minfo.fmap _cur_fn in
                let FunDec(_,_,Some rt,_,_) | CExtern(_,Some rt,_) = fdec.data in
                let rrt = p@>Ref (rt,p@>RW) in
                let replace =
                  [ (p@>Assign ((p@>Variable (p@>rval),rrt), e')) ;
                    (p@>Assign ((p@>Variable (p@>rctx),srbool), (p@>False,sbool))) ] in
                  p@>Block (p@>ListOfStuff replace, p@>End)
              end else
                p@>Return e'
          | VoidReturn ->
            if not (Stack.is_empty _secretflow && _tripped = Untripped) then begin
              if _tripped = Untripped then
                _tripped <- Pending;
              let replace =
                [ (p@>Assign ((p@>Variable (p@>rctx),srbool), (p@>False,sbool))) ] in
                p@>Block (p@>ListOfStuff replace, p@>End)
            end else
              p@>VoidReturn
          | _ -> super#next next_

  end

let transform m =
  let visit = new transret m in
    visit#fact_module ()
