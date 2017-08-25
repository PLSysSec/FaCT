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

let rec params_has_refs = function
  | [] -> false
  | ({data=Param(_,{data=vty'})}::params) ->
    begin
      match vty' with
        | RefVT(_,_,{data=mut}) ->
          (mut = Mut) || params_has_refs params
        | ArrayVT(_,_,{data=mut}) ->
          (mut = Mut) || params_has_refs params
    end

let fdec_has_refs {data=FunDec(_,_,params,_)} = params_has_refs params

let bty { data=(_,b) } = b
let r2bty { data=RefVT(b,ml,_) } = BaseET(b,ml)
let b2rty mut { data=BaseET(b,ml) } = RefVT(b,ml,mut)

#define sbool (BaseET(mkpos Bool, mkpos Fixed Secret))
#define sebool(e) (mkpos (e, sbool))
#define band(e1,e2) sebool(BinOp(Ast.LogicalAnd,e1,e2))
#define bor(e1,e2) sebool(BinOp(Ast.LogicalOr,e1,e2))
#define bnot(e1) sebool(UnOp(Ast.LogicalNot,e1))
#define bvar(x) sebool(Variable x)

#define rctx bvar((mkpos "__rnset"))
#define bctx (List.fold_left (fun x y -> band(x,y)) sebool(True) \
                (List.map (fun x -> bvar(x)) xf_ctx.ms))
#define ctx (band(band(bctx,rctx), \
                  if Env.has_var xf_ctx.venv (mkpos "__fctx") then bvar((mkpos "__fctx")) else sebool(True)))

#define ctx_select(e1,e2) (mkpos (Select(ctx,e1,e2), bty e2))

type xf_ctx_record = { rt:ret_type; fenv:function_dec Env.env; venv:variable_type Env.env; ms:var_name list }

let rec xf_arg' xf_ctx { data; pos=p } =
  match data with
    | ByValue e ->
      let e' = xf_expr xf_ctx e in
        ByValue e'
    | ByRef _ -> data
and xf_arg xf_ctx pa = { pa with data=xf_arg' xf_ctx pa }

and xf_expr' xf_ctx { data; pos=p } =
  let (e, ety) = data in
    match e with
      | True
      | False
      | IntLiteral _
      | Variable _
      | ArrayLen _ -> e
      | ArrayGet(x,e) ->
        let e' = xf_expr xf_ctx e in
          ArrayGet(x,e')
      | IntCast(bt,e) ->
        let e' = xf_expr xf_ctx e in
          IntCast(bt,e')
      | UnOp(op,e) ->
        let e' = xf_expr xf_ctx e in
          UnOp(op,e')
      | BinOp(op,e1,e2) ->
        let e1' = xf_expr xf_ctx e1 in
        let e2' = xf_expr xf_ctx e2 in
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
        let e1' = xf_expr xf_ctx e1 in
        let e2' = xf_expr xf_ctx e2 in
        let e3' = xf_expr xf_ctx e3 in
        if is_secret e1 then
          (* XXX e2 and e3 should be xf_expr'd with a secret ctx (matters for fn calls) *)
          Select(e1',e2',e3')
        else
          TernOp(e1',e2',e3')
      | Select _ -> raise @@ err(p)
      | FnCall(f,args) ->
        let args' = List.map (xf_arg xf_ctx) args in
        let fdec = Env.find_var xf_ctx.fenv f in
          if fdec_has_refs fdec then
            let fctx = ctx in
              FnCall(f,args'@[mkpos ByValue fctx])
          else
            FnCall(f,args')
      | Declassify e ->
        let e' = xf_expr xf_ctx e in
          Declassify e'
and xf_expr xf_ctx ({ data=(e,ety) } as pa) = { pa with data=(xf_expr' xf_ctx pa, ety) }

let venv_merge_up = function
  | Env.SubEnv(vtbl,venv) -> (* XXX need to check collisions *)
    let vtbl' = Env.get_vtbl venv in
      Hashtbl.iter (fun k v ->
                     Hashtbl.replace vtbl' k v)
        vtbl
  | _ -> raise @@ InternalCompilerError("Can't merge up topvenv")

let rec xf_stm' xf_ctx p = function
  | BaseDec(x,vt,e) ->
    let e' = xf_expr xf_ctx e in
      [BaseDec(x,vt,e')]
  | BaseAssign(x,e) ->
    let e' = xf_expr xf_ctx e in
    (* XXX also transform with fctx if we have one *)
    let should_transform = true in (* XXX *)
      if should_transform then
        let x' = mkpos (Variable x, r2bty (Env.find_var xf_ctx.venv x)) in
        let xfe' = ctx_select(e',x') in
          [BaseAssign(x,xfe')]
      else
        [BaseAssign(x,e')]
  | If(cond,thenstms,elsestms) ->
    let cond' = xf_expr xf_ctx cond in
      if is_secret cond' then
        let vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
        let tname = mkpos new_temp_var () in
        let mdec = BaseDec(tname, vt, cond') in
          Env.add_var xf_ctx.venv tname vt;
        let mnot = BaseAssign(tname, bnot(bvar(tname))) in
        let (tvenv,tstms) = thenstms in
        let (evenv,estms) = elsestms in
          venv_merge_up tvenv;
          venv_merge_up evenv;
        let xf_ctx' = { xf_ctx with ms=(tname::xf_ctx.ms) } in
        let thenstms' = List.flatten @@ List.map (xwrap @@ xf_stm' xf_ctx') tstms in
        let elsestms' = List.flatten @@ List.map (xwrap @@ xf_stm' xf_ctx') estms in
          [mdec] @ thenstms' @ [mnot] @ elsestms'
      else
        let thenstms' = xf_block xf_ctx.rt xf_ctx.fenv xf_ctx.ms thenstms in
        let elsestms' = xf_block xf_ctx.rt xf_ctx.fenv xf_ctx.ms elsestms in
        [If(cond',thenstms',elsestms')]
  | For(i,ity,lo,hi,stms) ->
    let lo' = xf_expr xf_ctx lo in
    let hi' = xf_expr xf_ctx hi in
    let stms' = xf_block xf_ctx.rt xf_ctx.fenv xf_ctx.ms stms in
      [For(i,ity,lo',hi',stms')]
  | VoidFnCall(f,args) ->
    let args' = List.map (xf_arg xf_ctx) args in
    let fdec = Env.find_var xf_ctx.fenv f in
      if fdec_has_refs fdec then
        let fctx = ctx in
          [VoidFnCall(f,args'@[mkpos ByValue fctx])]
      else
        [VoidFnCall(f,args')]
  | Return e ->
    let Some rt = xf_ctx.rt in
    let e' = xf_expr xf_ctx e in
    let should_transform = true in (* XXX *)
      if should_transform then
        let rval = mkpos "__rval" in
        let rnset = mkpos "__rnset" in
        let rnset' = bvar(rnset) in
        let assigned = bor(rnset',band(bctx,rctx)) in
        let rval' = mkpos (Variable rval, rt.data) in
        let xfe' = ctx_select(e',rval') in
          [BaseAssign(rval,xfe'); BaseAssign(rnset,assigned)]
      else
        [Return e']
  | VoidReturn ->
    let should_transform = true in (* XXX *)
      if should_transform then
        let rnset = mkpos "__rnset" in
        let rnset' = bvar(rnset) in
        let assigned = bor(rnset',band(bctx,rctx)) in
          [BaseAssign(rnset,assigned)]
      else
        [VoidReturn]
and xf_stm xf_ctx pa = List.map (make_ast pa.pos) (xf_stm' xf_ctx pa.pos pa.data)

and xf_block rt fenv ms (venv,stms) =
  let xf_ctx = { rt; fenv; venv; ms } in
  let stms' = List.flatten @@ List.map (xf_stm xf_ctx) stms in
    (venv, stms')

let xf_fdec fenv = pfunction
  | FunDec(f,rt,params,block) ->
    let (venv,stms) = block in
    let params' = if params_has_refs params
      then
        let fctx = mkpos "__fctx" in
        let fctx_vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
        let fctx_param = mkpos Param(fctx, fctx_vt) in
          Env.add_var venv fctx fctx_vt;
          params @ [fctx_param]
      else params in
    let venv,stms' = xf_block rt fenv [] (venv,stms) in
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
                FunDec(f,rt,params',(venv,rval_dec::stms'))
            | None ->
              FunDec(f,rt,params',(venv,stms'))
        end

let rec xf_fdecs fenv = function
  | [] -> []
  | (fdec::fdecs) ->
    let fdec' = xf_fdec fenv fdec in
    let {data=FunDec(fname,_,_,_)} = fdec' in
      Env.add_var fenv fname fdec';
      fdec'::(xf_fdecs fenv fdecs)

let xf_module (Module(_,fdecs)) =
  let fenv = Env.new_env () in
    Module(fenv, xf_fdecs fenv fdecs)


