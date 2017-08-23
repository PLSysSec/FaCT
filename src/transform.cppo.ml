open Pos
open Err
open Tast

#define err(p) InternalCompilerError("from source" ^ __LOC__ << p)

let wrap f pa = { pa with data=f pa.pos pa.data }
let xwrap f pa = f pa.pos pa.data

let rebind f pa = { pa with data=f pa }

#define mkpos make_ast p @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function



let new_temp_var =
  let ctr = ref 0 in
  let new_temp_var' () =
    ctr := !ctr + 1;
    "__m" ^ (string_of_int !ctr)
  in
  new_temp_var'

let is_secret e =
  let { data=(_,BaseET(_,{data=Fixed l})) } = e in
    l = Secret

let rec xf_arg' venv { data; pos=p } =
  match data with
    | ByValue e ->
      let e' = xf_expr venv e in
        ByValue e'
    | ByRef _ -> data
and xf_arg venv pa = { pa with data=xf_arg' venv pa }

and xf_expr' venv { data; pos=p } =
  let (e, ety) = data in
    match e with
      | True
      | False
      | IntLiteral _
      | Variable _
      | ArrayLen _ -> e
      | ArrayGet(x,e) ->
        let e' = xf_expr venv e in
          ArrayGet(x,e')
      | IntCast(bt,e) ->
        let e' = xf_expr venv e in
          IntCast(bt,e')
      | UnOp(op,e) ->
        let e' = xf_expr venv e in
          UnOp(op,e')
      | BinOp(op,e1,e2) ->
        let e1' = xf_expr venv e1 in
        let e2' = xf_expr venv e2 in
          if is_secret e1' then
            (* XXX e2 should be xf_expr'd with a secret ctx (matters for fn calls) *)
            match op with
              | Ast.LogicalAnd ->
                Select(e1',e2',mkpos (False, BaseET(mkpos Bool, mkpos Fixed Public)))
              | Ast.LogicalOr ->
                Select(e1',mkpos (True, BaseET(mkpos Bool, mkpos Fixed Public)),e2')
              | _ -> BinOp(op,e1',e2')
          else
            BinOp(op,e1',e2')
      | TernOp(e1,e2,e3) ->
        let e1' = xf_expr venv e1 in
        let e2' = xf_expr venv e2 in
        let e3' = xf_expr venv e3 in
        if is_secret e1 then
          (* XXX e2 and e3 should be xf_expr'd with a secret ctx (matters for fn calls) *)
          Select(e1',e2',e3')
        else
          TernOp(e1',e2',e3')
      | Select _ -> raise @@ err(p)
      | FnCall(f,args) ->
        let args' = List.map (xf_arg venv) args in
          (* XXX if there are any out params, need to pass down an fctx *)
          FnCall(f,args')
      | Declassify e ->
        let e' = xf_expr venv e in
          Declassify e'
and xf_expr venv ({ data=(e,ety) } as pa) = { pa with data=(xf_expr' venv pa, ety) }

#define sbool (BaseET(mkpos Bool, mkpos Fixed Secret))
#define sebool(e) (mkpos (e, sbool))
#define band(e1,e2) sebool(BinOp(Ast.LogicalAnd,e1,e2))
#define bor(e1,e2) sebool(BinOp(Ast.LogicalOr,e1,e2))
#define bnot(e1) sebool(UnOp(Ast.LogicalNot,e1))
#define bvar(x) sebool(Variable x)

#define rctx sebool(Variable (mkpos "__rnset"))
#define bctx (List.fold_left (fun x y -> band(x,y)) sebool(True) \
                (List.map (fun x -> bvar(x)) ms))

let venv_merge_up = function
  | Env.SubEnv(vtbl,venv) -> (* XXX need to check collisions *)
    let vtbl' = Env.get_vtbl venv in
      Hashtbl.iter (fun k v ->
                     Hashtbl.replace vtbl' k v)
        vtbl
  | _ -> raise @@ InternalCompilerError("Can't merge up topvenv")

let bty { data=(_,b) } = b
let r2bty { data=RefVT(b,ml,_) } = BaseET(b,ml)
let b2rty mut { data=BaseET(b,ml) } = RefVT(b,ml,mut)
#define ctx(e1,e2) (mkpos (Select(band(bctx,rctx),e1,e2), bty e2))

let rec xf_stm' rt venv ms p = function
  | BaseDec(x,vt,e) ->
    let e' = xf_expr venv e in
      [BaseDec(x,vt,e')]
  | BaseAssign(x,e) ->
    let e' = xf_expr venv e in
    (* XXX also transform with fctx if we have one *)
    let should_transform = true in (* XXX *)
      if should_transform then
        let x' = mkpos (Variable x, r2bty (Env.find_var venv x)) in
        let xfe' = ctx(e',x') in
          [BaseAssign(x,xfe')]
      else
        [BaseAssign(x,e')]
  | If(cond,thenstms,elsestms) ->
    let cond' = xf_expr venv cond in
      if is_secret cond' then
        let vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
        let tname = mkpos new_temp_var () in
        let mdec = BaseDec(tname, vt, cond') in
          Env.add_var venv tname vt;
        let mnot = BaseAssign(tname, bnot(bvar(tname))) in
        let (tvenv,tstms) = thenstms in
        let (evenv,estms) = elsestms in
          venv_merge_up tvenv;
          venv_merge_up evenv;
        let thenstms' = List.flatten @@ List.map (xwrap @@ xf_stm' rt venv (tname::ms)) tstms in
        let elsestms' = List.flatten @@ List.map (xwrap @@ xf_stm' rt venv (tname::ms)) estms in
          [mdec] @ thenstms' @ [mnot] @ elsestms'
      else
        let thenstms' = xf_block rt ms thenstms in
        let elsestms' = xf_block rt ms elsestms in
        [If(cond',thenstms',elsestms')]
  | For(i,ity,lo,hi,stms) ->
    let lo' = xf_expr venv lo in
    let hi' = xf_expr venv hi in
    let stms' = xf_block rt ms stms in
      [For(i,ity,lo',hi',stms')]
  | VoidFnCall(f,args) ->
    let args' = List.map (xf_arg venv) args in
      (* XXX if there are any out params, need to pass fctx as well *)
      [VoidFnCall(f,args')]
  | Return e ->
    let Some rt = rt in
    let e' = xf_expr venv e in
    let should_transform = true in (* XXX *)
      if should_transform then
        let rval = mkpos "__rval" in
        let rnset = mkpos "__rnset" in
        let rnset' = bvar(rnset) in
        let assigned = bor(rnset',band(bctx,rctx)) in
        let rval' = mkpos (Variable rval, rt.data) in
        let xfe' = ctx(e',rval') in
          [BaseAssign(rval,xfe'); BaseAssign(rnset,assigned)]
      else
        [Return e']
  | VoidReturn -> [VoidReturn] (* XXX set rnset *)
and xf_stm rt venv ms pa = List.map (make_ast pa.pos) (xf_stm' rt venv ms pa.pos pa.data)

and xf_block rt ms (venv,stms) =
  let stms' = List.flatten @@ List.map (xf_stm rt venv ms) stms in
    (venv, stms')

let xf_fdec fenv = pfunction
  | FunDec(f,rt,params,stms) ->
    let venv,stms' = xf_block rt [] stms in
    let rnset = mkpos "__rnset" in
    let rnset_vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
    let rnset_dec = mkpos BaseDec(rnset, rnset_vt, sebool(True)) in
      Env.add_var venv rnset rnset_vt;
      let stms' = rnset_dec::stms' in
        begin
          match rt with
            | Some et ->
              let rval = mkpos "__rval" in
              let rval_vt = mkpos (b2rty (mkpos Mut) et) in
              let rval_dec = mkpos BaseDec(rval, rval_vt, mkpos (IntLiteral 0,et.data)) in
                Env.add_var venv rval rval_vt;
                FunDec(f,rt,params,(venv,rval_dec::stms'))
            | None ->
              FunDec(f,rt,params,(venv,stms'))
        end

let xf_module (Module(fenv,fdecs)) =
  let fenv = Env.new_env () in
  let ret = Module(fenv, List.map (xf_fdec fenv) fdecs) in
    ret


