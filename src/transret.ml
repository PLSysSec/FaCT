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
    val mutable tripped : tripstate = Untripped

    method _is_everhi fn =
      let fdec = findfn _minfo.fmap fn in
        match fdec.data with
          | FunDec (_,{ everhi },_,_,_) -> everhi
          | _ -> false

    method fdec fdec =
      let p = fdec.pos in
        tripped <- Untripped;
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
                    let rvalvar = p @> VarDec (p@>rval,bty,zero) in
                    let ret = p @> Return (p@>Variable (p@>rval),bty) in
                      p@>ListOfStuff [rvalvar;rctx], ret
                  | None ->
                    let ret = p @> VoidReturn in
                      p@>ListOfStuff [rctx], ret in
              let blk' = (pre, p@>Block (replace_final_next blk ret)) in
                p @> FunDec (fn,fty,rt,params,blk')
            | CExtern _ -> fdec'

    method block (blk_,next) =
      let p = blk_.pos in
      let newblk,newnext =
        match blk_.data with
          | Scope _
          | If _
          | RangeFor _
          | ArrayFor _ ->
            let (blk_',next_') = super#block (blk_, next) in
              if tripped = Pending then
                tripped <- Tripped;
              let next' =
                if tripped = Tripped then
                  let rctx = (p@>Variable (p@>rctx),srbool) in
                  let rcond = (p@>Deref rctx,sbool) in
                  let nextthens = (p@>ListOfStuff [], next_') in
                  let nextelses = (p@>ListOfStuff [], p@>End) in
                  let newnextthing =
                    if next_'.data = End then ListOfStuff []
                    else If (rcond,nextthens,nextelses) in
                    p@>Block (p@>newnextthing, p@>End)
                else next_' in
                (blk_',next')
          | _ ->
            super#block (blk_,next)
      in
        if tripped = Tripped then
          let wrapblock blk =
            let rctx = (p@>Variable (p@>rctx),srbool) in
            let rcond = (p@>Deref rctx,sbool) in
            let emptyelses = (p@>ListOfStuff [], p@>End) in
              (p@>If (rcond,blk,emptyelses),p@>End) in
            match newblk.data with
              | RangeFor (x,bty,e1,e2,blk) ->
                (p@>RangeFor (x,bty,e1,e2,wrapblock blk),newnext)
              | ArrayFor (x,bty,e,blk) ->
                (p@>ArrayFor (x,bty,e,wrapblock blk),newnext)
              | _ -> newblk,newnext
        else
          newblk,newnext

    method next next_ =
      let p = next_.pos in
        match next_.data with
          | Return e ->
            let e' = visit#expr e in
              if not (Stack.is_empty _secretflow) then begin
                if tripped = Untripped then
                  tripped <- Pending;
                let fdec = findfn _minfo.fmap _cur_fn in
                let FunDec(_,_,Some rt,_,_) | CExtern(_,Some rt,_) = fdec.data in
                let replace =
                  [ (p@>Assign ((p@>Variable (p@>rval),rt), e')) ;
                    (p@>Assign ((p@>Variable (p@>rctx),srbool), (p@>False,sbool))) ] in
                  p@>Block (p@>ListOfStuff replace, p@>End)
              end else
                p@>Return e'
          | VoidReturn ->
            if not (Stack.is_empty _secretflow) then begin
              if tripped = Untripped then
                tripped <- Pending;
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
