open Util
open Pos
open Err
open Ast
open Ast_util
open Astmap

let findfn fmap fname =
  match Core.List.Assoc.find fmap fname ~equal:vequal with
    | Some fnty -> fnty
    | None -> raise @@ cerr fname.pos
                         "function not defined: '%s'"
                         fname.data


class array_spec_fdec =
  object (visit)
    inherit Astmap.ast_visitor as super

    method fdec_post =
      wrap @@ fun p -> function
        | FunDec (fn,ft,rt,params,body) ->
          let params' =
            List.map
              (fun param ->
                 match param.data with
                   | Param (x,{pos;data=Arr (bty,{data=LUnspecified},vattr)}) ->
                     let x_len = p @> make_fresh (x.data ^ "_len") in
                     let var_expr = p@>Variable x_len in
                       [p@>Param (x,{pos;data=Arr (bty,{pos;data=LExpression var_expr},vattr)});
                        p@>Param (x_len,{pos;data=UInt (64,p@>Public)})]
                   | _ -> [param])
              params
          in
            FunDec(fn,ft,rt,List.flatten params',body)
        | _ as fdec -> fdec

    end

class array_spec_fncall =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _fmap : (fun_name * params) list = []

    method module_pre m =
      let Module(_,fdecs) = m in
        List.iter
          (fun fdec ->
             match fdec.data with
               | FunDec(fn,_,_,params,_) ->
                 _fmap <- (fn,params) :: _fmap
               | _ -> ())
          fdecs;
        super#module_pre m

    method _fncall fn args =
      let params = findfn _fmap fn in
      let args' =
        List.map2
          (fun arg param ->
             let Param (x,bty) = param.data in
               if is_unspec_arr bty then
                 [arg ; arg.pos @> ArrayLen arg]
               else [arg])
          args
          params
      in
        List.flatten args'

    method stm_post ({pos=p;data} as stm_) =
      match data with
        | FnCall (x,bty,fn,args) ->
          p @> FnCall (x,bty,fn,visit#_fncall fn args)
        | VoidFnCall (fn,args) ->
          p @> VoidFnCall (fn,visit#_fncall fn args)
        | _ -> super#stm_post stm_

  end

let transform m =
  let visit_fncall = new array_spec_fncall in
  let m' = visit_fncall#fact_module m in
  let visit_fdec = new array_spec_fdec in
    visit_fdec#fact_module m'
