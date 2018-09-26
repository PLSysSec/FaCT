open Util
open Pos
open Err
open Ast
open Ast_util
open Astmap

let findfn fmap fname =
  Core.List.Assoc.find fmap fname ~equal:vequal

class array_spec_fncall =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _fmap : (fun_name * params) list = []

    method fact_module m =
      let Module(_,fdecs) = m in
        List.iter
          (fun fdec ->
             match fdec.data with
               | FunDec(fn,_,_,params,_) ->
                 _fmap <- (fn,params) :: _fmap
               | _ -> ())
          fdecs;
        super#fact_module m

    method _fncall fn args =
      match findfn _fmap fn with
        | Some params ->
          let args' =
            try List.map2
                  (fun arg param ->
                     let Param (x,bty) = param.data in
                       match is_unspec_arr bty with
                         | Some _ ->
                           [arg ; arg.pos @> ArrayLen arg]
                         | None -> [arg])
                  args
                  params
            with Invalid_argument _ ->
              raise @@ cerr fn.pos "arity mismatch on call to '%s'" fn.data
          in
            List.flatten args'
        | None -> args

    method stm_post ({pos=p;data} as stm_) =
      match data with
        | FnCall (x,bty,fn,args) ->
          p @> FnCall (x,bty,fn,visit#_fncall fn args)
        | VoidFnCall (fn,args) ->
          p @> VoidFnCall (fn,visit#_fncall fn args)
        | _ -> super#stm_post stm_

  end

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
        | CExtern (fn,ft,rt,params) as fdec ->
          List.iter
            (fun param ->
               match param.data with
                 | _ -> ())
            params;
          fdec

    end

let transform m =
  let visit_fncall = new array_spec_fncall in
  let m' = visit_fncall#fact_module m in
  let visit_fdec = new array_spec_fdec in
    visit_fdec#fact_module m'
