open Pos
open Err
open Tast
open Tast_utils
open Pseudocode

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

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
  rp    : label' ref;
  pc    : label';
  ms    : var_name list;
  rt    : ret_type;
  venv  : (var_name * variable_type) Env.env;
  fenv  : (function_dec * bool ref) Env.env;
  sdecs : struct_type list;
}

let rec params_has_secret_refs xf_ctx = function
  | [] -> false
  | ({data=Param(_,{data=vty'});pos=p}::params) ->
    begin
      match vty' with
        | RefVT(_,{data=Fixed label},{data=mut}) ->
          (mut = Mut && label = Secret) || params_has_secret_refs xf_ctx params
        | ArrayVT(_,{data=Fixed label},{data=mut}) ->
          (mut = Mut && label = Secret) || params_has_secret_refs xf_ctx params
        | StructVT(s,{data=mut}) ->
          (mut = Mut && struct_has_secrets xf_ctx.sdecs s) || params_has_secret_refs xf_ctx params
    end

let fdec_has_secret_refs xf_ctx p (fdec,everhi) =
  match fdec.data with
    | FunDec(_,_,_,params,_) -> (params_has_secret_refs xf_ctx params) && !everhi
    | CExtern(_,_,params) when !everhi -> raise @@ cerr("cannot call C extern from secret context", p)
    | _ -> false

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
#define blval(x) (mkpos (Base x,svbool))
#define bvar(x) sebool(Lvalue(blval(x)))

#define rctx sebool(Lvalue(mkpos (Base (mkpos "__rnset"),svbool)))
#define bctx (List.fold_left (fun x y -> band(x,y)) sebool(True) \
                (List.map (fun x -> bvar(x)) xf_ctx.ms))
#define ctx (band(band(bctx,rctx), \
                  if Env.has_var xf_ctx.venv (mkpos "__fctx") then bvar((mkpos "__fctx")) else sebool(True)))

let selty p e1 e2 =
  let BaseET(b',ml') = join_ty' p (type_of e1) (type_of e2) in
    BaseET(b',mkpos Fixed Secret)

#define ctx_select(e1,e2) (mkpos (Select(ctx,e1,e2), selty p e1 e2))

let rec xf_arg' xf_ctx { data; pos=p } =
  match data with
    | ByValue e ->
      let e' = xf_expr xf_ctx e in
        ByValue e'
    | ByRef lval -> ByRef (xf_lvalue xf_ctx lval)
    | ByArray(ae,m) ->
      let ae' = xf_arrayexpr xf_ctx ae in
        ByArray(ae',m)
and xf_arg xf_ctx pa = { pa with data=xf_arg' xf_ctx pa }

and xf_lvalue' xf_ctx { data=(lval,_); pos=p } =
  match lval with
    | Base _ as data -> data
    | ArrayEl(lval,e) ->
      let lval' = xf_lvalue xf_ctx lval in
      let e' = xf_expr xf_ctx e in
        ArrayEl(lval',e')
    | StructEl(lval,field) ->
      let lval' = xf_lvalue xf_ctx lval in
        StructEl(lval',field)
and xf_lvalue xf_ctx ({ data=(lval,vt); pos=p } as pa) =
  { pa with data=(xf_lvalue' xf_ctx pa, vt) }

and xf_expr' xf_ctx { data; pos=p } =
  let (e, ety) = data in
    match e with
      | True
      | False
      | IntLiteral _
      | Select _ -> e
      | Lvalue lval ->
        let lval' = xf_lvalue xf_ctx lval in
          Lvalue lval'
      | IntCast(bt,e) ->
        let e' = xf_expr xf_ctx e in
          IntCast(bt,e')
      | UnOp(op,e) ->
        let e' = xf_expr xf_ctx e in
          UnOp(op,e')
      | BinOp(op,e1,e2) ->
        if is_expr_secret e1 then
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
          if is_expr_secret e1' then
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
                       (xf_ctx.venv,[mkpos Assign(mkpos (Base res,resvt.data), e2)]),
                       (xf_ctx.venv,[mkpos Assign(mkpos (Base res,resvt.data), e3)])) in
            let stm' = xf_stm xf_ctx stm in
              Inject(res, resdec :: stm')
          else
            let e2' = xf_expr xf_ctx e2 in
            let e3' = xf_expr xf_ctx e3 in
              TernOp(e1',e2',e3')
      | FnCall(f,args) ->
        let args' = List.map (xf_arg xf_ctx) args in
        let fdec = Env.find_var xf_ctx.fenv f in
          if fdec_has_secret_refs xf_ctx p fdec then
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
      | ArrayNoinit _
      | ArrayCopy _ -> ae
      | ArrayView(x,e,lexpr) -> ArrayView(x,xf_expr xf_ctx e,lexpr)
      | ArrayComp(b,lexpr,x,e) -> ArrayComp(b,lexpr,x,xf_expr xf_ctx e)
and xf_arrayexpr xf_ctx ({ data=(ae,ety) } as pa) = { pa with data=(xf_arrayexpr' xf_ctx pa, ety) }

and xf_stm' xf_ctx p = function

  | BaseDec(x,vt,e) ->
    let e' = xf_expr xf_ctx e in
      [BaseDec(x,vt,e')]

  | ArrayDec(x,vt,ae) ->
    let ae' = xf_arrayexpr xf_ctx ae in
      [ArrayDec(x,vt,ae')]

  | StructDec(x,vt) ->
    [StructDec(x,vt)]

  | Assign(lval,e) ->
    let lval' = xf_lvalue xf_ctx lval in
    let e' = xf_expr xf_ctx e in
    let rpc = !(xf_ctx.rp) +$. xf_ctx.pc in
      if rpc = Secret then
        let _,vt = lval'.data in
        let rval' = mkpos (Lvalue lval', r2bty (mkpos vt)) in
        let xfe' = ctx_select(e',rval') in
          [Assign(lval',xfe')]
      else
        [Assign(lval',e')]

  | If(cond,thenstms,elsestms) ->
    let xf_sub l ms =
      let pc' = xf_ctx.pc +$. l in
      let (tvenv,tstms) = thenstms in
      let (fvenv,fstms) = elsestms in
      let xf_ctx1 = { xf_ctx with rp=ref !(xf_ctx.rp); pc=pc'; ms; venv=tvenv } in
      let xf_ctx2 = { xf_ctx with rp=ref !(xf_ctx.rp); pc=pc'; ms; venv=fvenv } in
      let thenstms' = xf_block xf_ctx1 tstms in
      let elsestms' = xf_block xf_ctx2 fstms in
        xf_ctx.rp := !(xf_ctx1.rp) +$. !(xf_ctx2.rp);
        thenstms', elsestms' in
    let cond' = xf_expr xf_ctx cond in
      if is_expr_secret cond' then
        let vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
        let tname = mkpos new_temp_var () in
        let mdec = BaseDec(tname, vt, cond') in
        let entry = (tname, vt) in
          Env.add_var xf_ctx.venv tname entry;
        let mnot = Assign(mkpos (Base tname,vt.data), bnot(bvar(tname))) in
        let ms = tname::xf_ctx.ms in
        let thenstms', elsestms' = xf_sub Secret ms in
          [mdec; Block(thenstms'); mnot; Block(elsestms')]
      else
        let thenstms', elsestms' = xf_sub Public xf_ctx.ms in
          [If(cond',thenstms',elsestms')]

  | For(i,ity,init,cond,upd,stms) ->
    let init' = xf_expr xf_ctx init in
    let cond' = xf_expr xf_ctx cond in
    let upd' = xf_expr xf_ctx upd in
    let (venv,ss) = stms in
    let xf_ctx' = { xf_ctx with venv; } in
    let stms' = xf_block xf_ctx' ss in
      [For(i,ity,init',cond',upd',stms')]

  | VoidFnCall(f,args) ->
    let args' = List.map (xf_arg xf_ctx) args in
    let fdec = Env.find_var xf_ctx.fenv f in
      if fdec_has_secret_refs xf_ctx p fdec then
        let fctx = ctx in
          [VoidFnCall(f,args'@[mkpos ByValue fctx])]
      else
        [VoidFnCall(f,args')]
  | DebugVoidFnCall _ as f -> [f]
  | Return e ->
    let rt =
      begin match xf_ctx.rt with
        | Some rt -> rt
        | _ -> raise @@ cerr("cannot return value from void function", p)
      end in
    let e' = xf_expr xf_ctx e in
    let rval = mkpos "__rval" in
    let b,ml = type_out rt in
    let vt = RefVT(b,ml,mkpos Const) in
    let rval_lval = mkpos (Base rval,vt) in
    let rval' = mkpos (Lvalue(rval_lval), rt.data) in
    let BaseET(_,rml) = rt.data in

    let rpc = !(xf_ctx.rp) +$. xf_ctx.pc in
      xf_ctx.rp := rpc;
    let should_transform = (rpc = Secret) in
      if should_transform then
        let xfe' = ctx_select(e',rval') in
        let xfeml = expr_to_ml xfe' in
          if not (xfeml <$ rml) then
            raise @@ cerr(Printf.sprintf "cannot return a %s expression from a %s function" (ps_label xfeml) (ps_label rml), e'.pos);
          let rnset = mkpos "__rnset" in
          let rnset' = bvar(rnset) in
          let assigned = ctx_select(sebool(False),rnset') in
            [ Assign(rval_lval,xfe'); Assign(blval(rnset),assigned) ]
      else
        let eml = expr_to_ml e in
          if not (eml <$ rml) then
            raise @@ cerr(Printf.sprintf "cannot return a %s expression from a %s function" (ps_label eml) (ps_label rml), e'.pos);
          [ Assign(rval_lval,e); Return rval' ]

  | VoidReturn ->
    xf_ctx.rp := !(xf_ctx.rp) +$. xf_ctx.pc;
    let rpc = !(xf_ctx.rp) +$. xf_ctx.pc in
    let should_transform = (rpc = Secret) in
      if should_transform then
        let rnset = mkpos "__rnset" in
        let rnset' = bvar(rnset) in
        let assigned = ctx_select(sebool(False),rnset') in
          [Assign(blval(rnset),assigned)]
      else
        [VoidReturn]

  | s -> print_endline @@ show_statement' s; raise @@ err(p)
and xf_stm xf_ctx pa = List.map (make_ast pa.pos) (xf_stm' xf_ctx pa.pos pa.data)

and xf_block xf_ctx stms =
  let stms' = List.flatten @@ List.map (xf_stm xf_ctx) stms in
    (xf_ctx.venv, stms')

let xf_fdec fenv sdecs everhi = pfunction
    | FunDec(f,ft,rt,params,block) ->
      let (venv,stms) = block in
      let xf_ctx = {
        rp=ref Public;
        pc=Public;
        rt;
        fenv;
        venv;
        sdecs;
        ms=[];
      } in
      let params' = if (params_has_secret_refs xf_ctx params) && !everhi
        then
          let fctx = mkpos "__fctx" in
          let fctx_vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
          let fctx_param = mkpos Param(fctx, fctx_vt) in
          let entry = (fctx, fctx_vt) in
            Env.add_var venv fctx entry;
            params @ [fctx_param]
        else params in
      let venv,stms' = xf_block xf_ctx stms in
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
                let rval_vt = mkpos RefVT(bty, l, mkpos Const) in
                let rval_dec = mkpos BaseDec(rval, rval_vt, mkpos (def_val,et)) in
                let ret = mkpos Return (mkpos (Lvalue(mkpos (Base rval, rval_vt.data)), et)) in
                let stms'' = if !(xf_ctx.rp) = Secret then stms'@[ret] else stms' in
                  FunDec(f,ft,rt,params',(venv,rval_dec::stms''))
              | None ->
                FunDec(f,ft,rt,params',(venv,stms'))
          end
    | CExtern _ as fdec -> fdec

let rec xf_fdecs fenv sdecs = function
  | [] -> []
  | (fdec::fdecs) ->
    let fname = fname_of fdec in
    let (_,everhi) = Env.find_var fenv fname in
    let fdec' = xf_fdec fenv sdecs everhi fdec in
      Env.replace_var fenv fname (fdec', everhi);
      fdec'::(xf_fdecs fenv sdecs fdecs)

let xf_module (Module(fenv,fdecs,sdecs)) =
  Module(fenv, xf_fdecs fenv sdecs fdecs, sdecs)


