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

type xf_ctx_record = {
  rp   : label' ref;
  pc   : label';
  ms   : var_name list;
  rt   : ret_type;
  venv : (var_name * variable_type) Env.env;
  fenv : function_dec Env.env;
}

let is_secret e =
  let { data=(_,BaseET(_,{data=Fixed l})) } = e in
    l = Secret

let is_var_secret xf_ctx x =
  let _,vt = Env.find_var xf_ctx.venv x in
    match vt.data with
      | RefVT(_,ml,_)
      | ArrayVT(_,ml,_) ->
        let Fixed l = ml.data in
          l = Secret

let rec params_has_secret_refs = function
  | [] -> false
  | ({data=Param(_,{data=vty'})}::params) ->
    begin
      match vty' with
        | RefVT(_,{data=Fixed label},{data=mut}) ->
          (mut = Mut && label == Secret) || params_has_secret_refs params
        | ArrayVT(_,{data=Fixed label},{data=mut}) ->
          (mut = Mut && label == Secret) || params_has_secret_refs params
    end

let fdec_has_secret_refs = xfunction
  | FunDec(_,_,params,_) -> params_has_secret_refs params
  | CExtern(_,_,params) -> false

let bty { data=(_,b) } = b
let r2bty { data=RefVT(b,ml,_) } = BaseET(b,ml)
let a2bty { data=ArrayVT({data=ArrayAT(b,_)},ml,_) } = BaseET(b,ml)
let b2rty l mut { data=BaseET(b,_); pos=p } =
  let ml = mkpos Fixed l in
    RefVT(b,ml,mut)

#define sbool (BaseET(mkpos Bool, mkpos Fixed Secret))
#define svbool (RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const))
#define sebool(e) (mkpos (e, sbool))
#define band(e1,e2) sebool(BinOp(Ast.LogicalAnd,e1,e2))
#define bor(e1,e2) sebool(BinOp(Ast.LogicalOr,e1,e2))
#define bnot(e1) sebool(UnOp(Ast.LogicalNot,e1))
#define bvar(x) sebool(Variable x)

#define rctx sebool(Variable(mkpos "__rnset"))
#define bctx (List.fold_left (fun x y -> band(x,y)) sebool(True) \
                (List.map (fun x -> bvar(x)) xf_ctx.ms))
#define ctx (band(band(bctx,rctx), \
                  if Env.has_var xf_ctx.venv (mkpos "__fctx") then bvar((mkpos "__fctx")) else sebool(True)))

#define ctx_select(e1,e2) (mkpos (Select(ctx,e1,e2), bty e2))

let (<$) l1 l2 =
  match l1,l2 with
    | x, y when x = y -> true
    | Public, Secret -> true
    | _ -> false

let join_ml l1 l2 =
  match l1,l2 with
    | Public, Public -> Public
    | Public, Secret
    | Secret, Public
    | Secret, Secret -> Secret

let rec xf_arg' xf_ctx { data; pos=p } =
  match data with
    | ByValue e ->
      let e' = xf_expr xf_ctx e in
        ByValue e'
    | ByRef _ -> data
    | ByArray(ae,m) ->
      let ae' = xf_arrayexpr xf_ctx ae in
        ByArray(ae',m)
and xf_arg xf_ctx pa = { pa with data=xf_arg' xf_ctx pa }

and xf_expr' xf_ctx { data; pos=p } =
  let (e, ety) = data in
    match e with
      | True
      | False
      | IntLiteral _
      | Variable _
      | Select _ -> e
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
        if is_secret e1 then
          match op with
            | Ast.LogicalAnd ->
              let { data=(expr,ety) } = xf_expr xf_ctx (mkpos (TernOp(e1,e2,sebool(False)), sbool)) in
                expr
            | Ast.LogicalOr ->
              let { data=(expr,ety) } = xf_expr xf_ctx (mkpos (TernOp(e1,sebool(True),e2), sbool)) in
                expr
            | _ ->
              let e1' = xf_expr xf_ctx e1 in
              let e2' = xf_expr xf_ctx e2 in
                BinOp(op,e1',e2')
        else
          let e1' = xf_expr xf_ctx e1 in
          let e2' = xf_expr xf_ctx e2 in
            BinOp(op,e1',e2')
      | TernOp(e1,e2,e3) ->
        let e1' = xf_expr xf_ctx e1 in
          if is_secret e1' then
            let res = mkpos new_temp_var () in
            let resvt = mkpos b2rty Secret (mkpos Const) (mkpos ety) in
            let def_val =
              match ety with
                | BaseET({data=Bool},_) -> False
                | _ -> IntLiteral 0
            in
            let resdec = mkpos BaseDec(res, resvt, mkpos (def_val, ety)) in
            let entry = (res, resvt) in
              Env.add_var xf_ctx.venv res entry;
            let stm =
              mkpos If(e1,
                       (xf_ctx.venv,[mkpos BaseAssign(res, e2)]),
                       (xf_ctx.venv,[mkpos BaseAssign(res, e3)])) in
            let stm' = xf_stm xf_ctx stm in
              Inject(res, resdec :: stm')
          else
            let e2' = xf_expr xf_ctx e2 in
            let e3' = xf_expr xf_ctx e3 in
              TernOp(e1',e2',e3')
      | FnCall(f,args) ->
        let args' = List.map (xf_arg xf_ctx) args in
        let fdec = Env.find_var xf_ctx.fenv f in
          if fdec_has_secret_refs fdec then
            let fctx = ctx in
              FnCall(f,args'@[mkpos ByValue fctx])
          else
            FnCall(f,args')
      | DebugFnCall _ as f -> f
      | Declassify e ->
        let e' = xf_expr xf_ctx e in
          Declassify e'
and xf_expr xf_ctx ({ data=(e,ety) } as pa) = { pa with data=(xf_expr' xf_ctx pa, ety) }

and xf_arrayexpr' xf_ctx { data; pos=p } =
  let (ae, ety) = data in
    match ae with
      | ArrayLit exprs -> ArrayLit(List.map (xf_expr xf_ctx) exprs)
      | ArrayVar _
      | ArrayZeros _
      | ArrayCopy _ -> ae
      | ArrayView(x,e,lexpr) -> ArrayView(x,xf_expr xf_ctx e,lexpr)
      | ArrayComp(b,lexpr,x,e) -> ArrayComp(b,lexpr,x,xf_expr xf_ctx e)
and xf_arrayexpr xf_ctx ({ data=(ae,ety) } as pa) = { pa with data=(xf_arrayexpr' xf_ctx pa, ety) }

and xf_stm' xf_ctx p = function

  | BaseDec(x,vt,e) ->
    let e' = xf_expr xf_ctx e in
      [BaseDec(x,vt,e')]

  | BaseAssign(x,e) ->
    if is_var_secret xf_ctx x then
      let e' = xf_expr xf_ctx e in
      let _,vt = Env.find_var xf_ctx.venv x in
      let x' = mkpos (Variable x, r2bty vt) in
      let xfe' = ctx_select(e',x') in
        [BaseAssign(x,xfe')]
    else
      [BaseAssign(x,e)]

  | ArrayDec(x,vt,ae) ->
    let ae' = xf_arrayexpr xf_ctx ae in
      [ArrayDec(x,vt,ae')]

  | ArrayAssign(x,n,e) ->
    if is_var_secret xf_ctx x then
      let n' = xf_expr xf_ctx n in
      let e' = xf_expr xf_ctx e in
      let _,vt = Env.find_var xf_ctx.venv x in
      let x' = mkpos (ArrayGet(x,n'), a2bty vt) in
      let xfe' = ctx_select(e',x') in
        [ArrayAssign(x,n',xfe')]
    else
      [ArrayAssign(x,n,e)]

  | If(cond,thenstms,elsestms) ->
    let cond' = xf_expr xf_ctx cond in
      if is_secret cond' then
        let vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
        let tname = mkpos new_temp_var () in
        let mdec = BaseDec(tname, vt, cond') in
        let entry = (tname, vt) in
          Env.add_var xf_ctx.venv tname entry;
        let mnot = BaseAssign(tname, bnot(bvar(tname))) in
        let xf_ctx' = { xf_ctx with ms=(tname::xf_ctx.ms) } in
        let thenstms' = xf_block xf_ctx'.rt xf_ctx'.fenv xf_ctx'.ms thenstms in
        let elsestms' = xf_block xf_ctx'.rt xf_ctx'.fenv xf_ctx'.ms elsestms in
          [mdec; Block(thenstms'); mnot; Block(elsestms')]
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
      if fdec_has_secret_refs fdec then
        let fctx = ctx in
          [VoidFnCall(f,args'@[mkpos ByValue fctx])]
      else
        [VoidFnCall(f,args')]
  | DebugVoidFnCall _ as f -> [f]
  | Return e ->
    let Some rt = xf_ctx.rt in
    let e' = xf_expr xf_ctx e in
    let rval = mkpos "__rval" in
    let rval' = mkpos (Variable rval, rt.data) in
    let xfe' = ctx_select(e',rval') in
    let rassign = BaseAssign(rval,xfe') in
    let should_transform = (List.length xf_ctx.ms > 0) in
      [ BaseAssign(rval,xfe');
        if should_transform then
          let rnset = mkpos "__rnset" in
          let rnset' = sebool(Variable(rnset)) in
          let assigned = ctx_select(sebool(False),rnset') in
            BaseAssign(rnset,assigned)
        else
          Return rval' ]

  | VoidReturn ->
    let should_transform = (List.length xf_ctx.ms > 0) in
      if should_transform then
        let rnset = mkpos "__rnset" in
        let rnset' = sebool(Variable(rnset)) in
        let assigned = ctx_select(sebool(False),rnset') in
          [BaseAssign(rnset,assigned)]
      else
        [VoidReturn]

  | s -> print_endline @@ show_statement' s; raise @@ err(p)
and xf_stm xf_ctx pa = List.map (make_ast pa.pos) (xf_stm' xf_ctx pa.pos pa.data)

and xf_block rt fenv ms (venv,stms) =
  let xf_ctx = { rp=ref Public; pc=Public; rt; fenv; venv; ms } in
  let stms' = List.flatten @@ List.map (xf_stm xf_ctx) stms in
    (venv, stms')

let xf_fdec fenv = pfunction
  | FunDec(f,rt,params,block) ->
    let (venv,stms) = block in
    let params' = if params_has_secret_refs params
      then
        let fctx = mkpos "__fctx" in
        let fctx_vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
        let fctx_param = mkpos Param(fctx, fctx_vt) in
        let entry = (fctx, fctx_vt) in
          Env.add_var venv fctx entry;
          params @ [fctx_param]
      else params in
    let venv,stms' = xf_block rt fenv [] (venv,stms) in
    let rnset = mkpos "__rnset" in
    let rnset_dec = mkpos BaseDec(rnset, mkpos svbool, sebool(True)) in
      let stms' = rnset_dec::stms' in
        begin
          match rt with
            | Some {data=et} ->
              let rval = mkpos "__rval" in
              let BaseET(bty,l) = et in
              let def_val =
                  match bty.data with
                    | Bool -> False
                    | _ -> IntLiteral 0
              in
              let rval_dec = mkpos BaseDec(rval, mkpos RefVT(bty, l, mkpos Const), mkpos (def_val,et)) in
              let ret = mkpos Return (mkpos (Variable(rval), et)) in
                FunDec(f,rt,params',(venv,rval_dec::stms'))
            | None ->
              FunDec(f,rt,params',(venv,stms'))
        end
  | CExtern _ as fdec -> fdec

let rec xf_fdecs fenv = function
  | [] -> []
  | (fdec::fdecs) ->
    let fdec' = xf_fdec fenv fdec in
      begin
        match fdec'.data with
          | FunDec(fname,_,_,_)
          | CExtern(fname,_,_) ->
            Env.add_var fenv fname fdec';
            fdec'::(xf_fdecs fenv fdecs)
      end

let xf_module (Module(_,fdecs)) =
  let fenv = Env.new_env () in
    Module(fenv, xf_fdecs fenv fdecs)


