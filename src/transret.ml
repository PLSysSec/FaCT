open Util
open Pos
open Err
open Tast
open Tast_util
open Transmap

let srbool = fake_pos @> Ref (sbool, fake_pos @> RW)

class transret m =
  object (visit)
    inherit Transmap.transmap m as super

    method _is_everhi fn =
      let fdec = findfn _minfo.fmap fn in
        match fdec.data with
          | FunDec (_,{ everhi },_,_,_) -> everhi
          | _ -> false

    method fdec fdec =
      let p = fdec.pos in
      let fdec' = super#fdec fdec in
        match fdec'.data with
          | FunDec (fn,fty,rt,params,blk) ->
            let btrue = (p@>True,sbool) in
            let reftrue = (p@>Enref btrue,srbool) in
            let rctx = (p @> VarDec (p@>"__rctx",srbool,reftrue), p@>Public) in
            let blk' =
              rctx ::
              match rt with
                | Some bty ->
                  let zero =
                    (p@>(match bty.data with
                           | Bool _ -> False
                           | UInt _ | Int _ -> IntLiteral 0
                           | _ -> raise @@ err p)
                    ,bty)
                  in
                  let rval = (p @> VarDec (p@>"__rval",bty,zero), p@>Public) in
                  let ret = (p @> Return (p@>Variable (p@>"__rval"),bty), p@>Public) in
                    rval :: (blk @ [ret])
                | None ->
                  let ret = (p @> VoidReturn, p@>Public) in
                    blk @ [ret] in
              p @> FunDec (fn,fty,rt,params,blk')
          | CExtern _ -> fdec'

    method stm_post (stm_,lbl_) =
      let p = stm_.pos in
      let stms' =
        match stm_.data with
          | Return e ->
            let fdec = findfn _minfo.fmap _cur_fn in
            let FunDec(_,_,Some rt,_,_) | CExtern(_,Some rt,_) = fdec.data in
              [ (p@>Assign ((p@>Variable (p@>"__rval"),rt), e)) ;
                (p@>Assign ((p@>Variable (p@>"__rctx"),srbool), (p@>False,sbool))) ]
          | VoidReturn ->
            [ (p@>Assign ((p@>Variable (p@>"__rctx"),srbool), (p@>False,sbool))) ]
          | _ -> [] in
        match stms' with
          | [] -> super#stm_post (stm_,lbl_)
          | _ -> List.map (fun stm -> (stm,lbl_)) stms'

  end

let transform m =
  let visit = new transret m in
    visit#fact_module ()
