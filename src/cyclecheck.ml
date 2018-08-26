open Util
open Pos
open Err
open Ast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

class cyclechecker m =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _edges : fun_name list StringMap.t = StringMap.empty

    method _edges () = _edges

    method _fncall fn =
      let fns = StringMap.find_opt
                  _cur_fn.data
                  _edges
                >!> [] in
        _edges <- StringMap.add
                    _cur_fn.data
                    (fn :: fns)
                    _edges

    method stm_post stm_ =
      begin
        match stm_.data with
          | FnCall (_,_,fn,_)
          | VoidFnCall (fn,_) ->
            visit#_fncall fn
          | _ -> ()
      end;
      super#stm_post stm_

    method expr_post e_ =
      begin
        match e_.data with
          | FnCallExpr(fn,_) ->
            visit#_fncall fn
          | _ -> ()
      end;
      super#expr_post e_

  end

let transform m =
  let visit = new cyclechecker () in
  let m' = visit#fact_module m in
  let Module(sdecs,fdecs) = m' in
  let edges = visit#_edges () in
  let sorted = ref [] in
  let perm_marked  = ref StringSet.empty in
  let rec visit p callstack =
    let caller = List.hd callstack in
      if not @@ StringSet.mem caller !perm_marked then
        begin
          if List.mem caller (List.tl callstack) then
            raise @@ cerr p
                       "recursion detected: %s"
                       (List.fold_left
                          (fun str fn -> fn ^ " -> " ^ str)
                          caller
                          (List.tl callstack));
          let callees = StringMap.find_opt caller edges >!> [] in
            List.iter
              (fun callee -> visit callee.pos (callee.data::callstack))
              callees;
            perm_marked := StringSet.add caller !perm_marked;
            sorted := (p@>caller) :: !sorted
        end
  in
    StringMap.iter
      (fun caller _ -> visit fake_pos [caller])
      edges;
    let fdecs' =
      (* topological sort, callers first *)
      Base.List.filter_map
        !sorted
        (fun fn ->
           try
             List.find_opt
               (function
                 | {data=FunDec(fn',_,_,_,_) | CExtern(fn',_,_,_)} when fn'.data = fn.data -> true
                 | _ -> false)
               fdecs
           with
               Not_found ->
               if not (List.mem fn.data Stdlib.names) then
                 raise @@ cerr fn.pos
                            "unknown function: '%s'"
                            fn.data
               else None) in
    let standalones =
      List.filter
        (fun {data=FunDec(fn',_,_,_,_)|CExtern(fn',_,_,_)} ->
           not @@ List.mem fn'.data (List.map (fun {data} -> data) !sorted))
        fdecs in
      Module(sdecs,fdecs' @ standalones)
