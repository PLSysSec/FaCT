open Util
open Pos
open Err
open Ast
open Ast_util
open Astmap

class fn_extractor =
  object (visit)
    inherit Astmap.ast_visitor as super
    val _frets : (fun_name * ret_type) mlist = ref []

    method fact_module m =
      let Module(_,fdecs) = m in
        List.iter
          (fun fdec ->
             match fdec.data with
               | FunDec(fn,_,rt,_,_)
               | CExtern(fn,_,rt,_) ->
                 mlist_push (fn,rt) _frets)
          fdecs;
        super#fact_module m

    method expr_post =
      wrap @@ fun p -> function
        | FnCallExpr (fn,args) ->
          let var = p@>(make_fresh fn.data) in
            (mlist_find ~equal:vequal !_frets fn >>= fun rt ->
             match rt with
               | None -> raise @@ cerr p
                                    "cannot use void function in expression: %s"
                                    fn.data
               | Some rt' ->
                 let fcall = p@>FnCall(var,rt',fn,args) in
                   _pre_inject <- fcall :: _pre_inject;
                   Some (Variable var))
            >!!> err p
        | e -> e

  end

let transform m =
  let visit = new fn_extractor in
    visit#fact_module m
