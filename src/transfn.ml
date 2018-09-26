open Util
open Pos
open Err
open Tast
open Tast_util
open Transmap

let fctx = "__fctx"

class transfn m =
  object (visit)
    inherit Transmap.transmap m as super

    method _is_everhi fn =
      let fdec = findfn _minfo.fmap fn in
        match fdec.data with
          | FunDec (_,{ everhi },_,_,_)
          | StdlibFn (_,{ everhi },_,_) -> everhi
          | _ -> false

    method fdec fdec =
      let p = fdec.pos in
      let fdec' = super#fdec fdec in
        match fdec'.data with
          | FunDec (fn,fty,rt,params,blk) ->
            let params',blk' =
              if fty.everhi then
                let params' = params @ [ p@>Param (p@>fctx, p@>Bool (p@>Secret)) ] in
                let cond =
                  (p@>Variable (p@>fctx), p@>Bool (p@>Secret)) in
                let empty = (p@>ListOfStuff [], p@>End) in
                let blk' = (p@>If (cond,blk,empty), p@>End) in
                  params',blk'
              else params,blk in
              p @> FunDec (fn,fty,rt,params',blk')
          | CExtern _ -> fdec'
          | StdlibFn (fn,fty,rt,params) ->
            let params' =
              if fty.everhi then
                params @ [ p@>Param (p@>fctx, p@>Bool (p@>Secret)) ]
              else params in
              p @> StdlibFn (fn,fty,rt,params')

    method stm stm_ =
      let p = stm_.pos in
      let stm' =
        match stm_.data with
          | FnCall (x,bty,fn,args)
            when visit#_is_everhi fn ->
            let args' = args @ [ __ctx ] in
              Some (p@>FnCall (x,bty,fn,args'))
          | VoidFnCall (fn,args)
            when visit#_is_everhi fn ->
            let args' = args @ [ __ctx ] in
              Some (p@>VoidFnCall (fn,args'))
          | _ -> None in
      let stm' =
        match stm' with
          | Some stm -> stm
          | None -> stm_
      in
        super#stm stm'

  end

let transform m =
  let visit = new transfn m in
    visit#fact_module ()
