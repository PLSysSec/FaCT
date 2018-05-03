open Pos
open Err
open Tast

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

let fake_pos = { file=""; line=0; lpos=0; rpos=0 }

#define mkpos make_ast p @@
#define mkp(pa) make_ast pa.pos @@
#define mkfake make_ast fake_pos @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function

let smack_assume = "__VERIFIER_assume"
let smack_assert = "__VERIFIER_assert"

let get_nondet = pfunction
  | Int 8   -> "__VERIFIER_nondet_signed_char"
  | Int 16  -> "__VERIFIER_nondet_signed_short"
  | Int 32  -> "__VERIFIER_nondet_signed_int"
  | Int 64  -> "__VERIFIER_nondet_signed_long_long"
  | UInt 8  -> "__VERIFIER_nondet_unsigned_char"
  | UInt 16 -> "__VERIFIER_nondet_unsigned_short"
  | UInt 32 -> "__VERIFIER_nondet_unsigned_int"
  | UInt 64 -> "__VERIFIER_nondet_unsigned_long_long"

let int64ety = BaseET(mkfake Int 64, mkfake Fixed Public)

let abinop op e1 e2 =
  let p = e1.pos in
    mkpos (BinOp(op, e1, e2), int64ety)

let binop op e1 e2 =
  let p = e1.pos in
  let ebool = BaseET(mkpos Bool, mkpos Fixed Public) in
    mkpos (BinOp(op, e1, e2), ebool)

let call_assume cond =
  let p = cond.pos in
  let arg = mkpos ByValue cond in
    mkpos VoidFnCall(mkpos smack_assume, [arg])

let call_assert cond =
  let p = cond.pos in
  let arg = mkpos ByValue cond in
    mkpos VoidFnCall(mkpos smack_assert, [arg])

let make_signed e =
  let e',ety = e.data in
  let BaseET(bty,ml) = ety in
    match bty.data with
      | UInt n ->
        let newty = {bty with data=Int n} in
        let ety' = BaseET(newty,ml) in
          {e with data=(IntCast(newty,e), ety)}
      | Num (n,s) ->
        {e with data=(e', BaseET(mkfake Num (n, true), mkfake Fixed Public))}
      | Int _ -> e

let one = mkfake LIntLiteral 1

class smack_visitor =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _venv = Env.new_env () (* hacky *)
    val mutable _fenv = Env.new_env () (* hacky *)
    val mutable _sdecs = []
    val mutable _inits = []
    val mutable _paraminits = []
    val mutable _arrsetting = false
    val mutable _fins = []
    val mutable _clobbers = []

    method lval_to_id lval =
      let (lval',_) = lval.data in
        match lval' with
          | Base n -> "#" ^ n.data
          | ArrayEl _ -> "####" (* XXX *)
          | StructEl(_lval,field) ->
            visit#lval_to_id _lval ^ "#" ^ field.data

    method lval_is_uninit lval =
      let id = visit#lval_to_id lval in
        List.mem_assoc id _inits

    method lval_get_init lval =
      let id = visit#lval_to_id lval in
        List.assoc id _inits

    method declare_init x vty =
      let lvalinit = "#" ^ x.data in
      let bvty = mkfake RefVT(mkfake Int 64, mkfake Fixed Public, mkfake Const) in
      let lvalinitlval = (mkfake (Base (mkfake lvalinit), bvty.data)) in
      let init_n = mkfake Tast_utils.make_nlit fake_pos 0 in
        _inits <- (lvalinit, (lvalinitlval, init_n)) :: _inits;
        mkfake BaseDec(mkfake lvalinit, bvty, init_n)

    method lexpr_to_expr lexpr =
      match lexpr.data with
        | LIntLiteral n ->
          (* assuming x64 architecture *)
          mkfake (IntLiteral n, int64ety)
        | LDynamic len ->
          let _,len_vty = Env.find_var _venv len in
          let RefVT(bty,ml,_) = len_vty.data in
          let len_ety = BaseET(bty, mkfake (Fixed Public)) in
          let base = mkfake (Base len, len_vty.data) in
          let expr = mkfake (Lvalue base, len_ety) in
          (* assuming x64 architecture *)
          let bty = mkfake (Int 64) in
            mkfake (IntCast(bty, expr), BaseET(bty, mkfake (Fixed Public)))

    method check_arrget lval n l =
      let l' = visit#lexpr_to_expr l in
      let lvalinit,_ = visit#lval_get_init lval in
      let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
        [ call_assert(binop Ast.GTE lvalinit'
                        (abinop Ast.Plus n l')) ]

    method check_arrset lval n =
      let lvalinit,_ = visit#lval_get_init lval in
      let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
        [ call_assert(binop Ast.GTE lvalinit' n) ]

    method finish_arrset lval n l =
      let l' = visit#lexpr_to_expr l in
      let lvalinit,base = visit#lval_get_init lval in
      let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
      let add = abinop Ast.Plus n l' in
      let lvalinit' = abinop Ast.Plus lvalinit' base in
      let add = abinop Ast.Plus add base in
      let cond = binop Ast.GT lvalinit' add in
      let sel = mkfake (TernOp(cond, lvalinit', add), int64ety) in
        [ mkfake Assign(lvalinit, sel) ]


    method fact_module m =
      let Module(fenv,fdecs,sdecs) = m in
        _fenv <- fenv;
      let p = fake_pos in (* for mkpos macro *)
      let i32param = mkpos Param(mkpos "predicate", mkpos RefVT(mkpos Int 32, mkpos Fixed Public, mkpos Const), default_param_attr) in

      let fdecs' = fdecs in

      let fdec = mkpos CExtern(mkpos smack_assume, None, [i32param]) in
      let fdecs' = fdec::fdecs' in
        (*Env.add_var fenv (mkpos smack_assume) (fdec, ref false);*)

      let fdec = mkpos CExtern(mkpos smack_assert, None, [i32param]) in
      let fdecs' = fdec::fdecs' in
        (*Env.add_var fenv (mkpos smack_assert) (fdec, ref false);*)

        let fdecs' = List.fold_left
                       (fun fdecs ty ->
                          let nondet = get_nondet (mkpos ty) in
                          let rty = mkpos BaseET(mkpos ty, mkpos Fixed Public) in
                          let fdec = mkpos CExtern(nondet, Some rty, []) in
                          let fdecs' = fdec::fdecs in
                            (*Env.add_var fenv nondet (fdec, ref false);*)
                            fdecs')
                       fdecs'
                       [ UInt 64; UInt 32; UInt 16; UInt 8;
                         Int 64;  Int 32;  Int 16;  Int 8; ] in

      let m' = Module(fenv, fdecs', sdecs) in
        _sdecs <- sdecs;
        super#fact_module m'

    method fdec fdec_ =
      _clobbers <- [];
      super#fdec fdec_

    method block ((venv,stms) as block) =
      _venv <- venv;
      let decinits =
        List.flatten
          (List.map
             (fun param ->
                match param.data with
                  | Param(x,({data=ArrayVT _} as vty),attr) ->
                    if attr.output_only then
                      begin
                        _clobbers <- x :: _clobbers;
                        [visit#declare_init x vty]
                      end
                    else [])
             _paraminits) in
        _paraminits <- [];
        let stms' =
          decinits @ stms in
          super#block (venv,stms')

    method lval ({data=(lval,vty); pos=p} as lval_) =
      match lval with
        | ArrayEl(lval,e) when visit#lval_is_uninit lval ->
          let e' = make_signed e in
            mkpos (CheckedLval(visit#check_arrget lval e' one, super#lval lval_), vty)
        | _ -> super#lval lval_

    method lval_set ({data=(lval,vty); pos=p} as lval_) =
      match lval with
        | ArrayEl(lval,e) when visit#lval_is_uninit lval ->
          let e' = make_signed e in
            (mkpos (CheckedLval(visit#check_arrset lval e', super#lval lval_), vty),
             visit#finish_arrset lval e' one)
        | _ -> (super#lval lval_, [])

    method aexpr ({data=(ae,ety); pos=p} as ae_) =
      match ae with
        | ArrayVar lval when visit#lval_is_uninit lval ->
          let _,vty = lval.data in
          let lexpr = Tast_utils.refvt_to_lexpr (mkfake vty) in
          let zero = (mkfake Tast_utils.make_nlit fake_pos 0) in
          let check = if _arrsetting then
              (_fins <- visit#finish_arrset lval zero lexpr;
               visit#check_arrset lval zero)
            else
              visit#check_arrget lval zero lexpr
          in
            mkpos (CheckedArrayExpr(check, super#aexpr ae_), ety)
        | ArrayView(lval,e,lexpr) when visit#lval_is_uninit lval ->
          let e' = make_signed e in
          let check = if _arrsetting then
              (_fins <- visit#finish_arrset lval e' lexpr;
               visit#check_arrset lval e')
            else
              visit#check_arrget lval e' lexpr
          in
            mkpos (CheckedArrayExpr(check, super#aexpr ae_), ety)
        | _ ->
          _fins <- []; super#aexpr ae_

    method do_fn f args =
      let fn,_ = Env.find_var _fenv f in
      let params =
        match fn.data with
          | FunDec(_,_,_,params,_) -> params
          | CExtern(_,_,params) -> params
          | StdlibFunDec(_,_,_,params) -> params in
      let pairs = List.combine args params in
      let args' =
        List.map
          (fun (arg, param) ->
             let Param(x,vty,attr) = param.data in
               match vty.data with
                 | ArrayVT(_,_,m,_) ->
                   if m.data = Mut && attr.output_only then
                     let old = _arrsetting in
                       _arrsetting <- true;
                       let arg' = visit#arg arg in
                         _arrsetting <- old;
                         let r = (arg', _fins) in
                           _fins <- [];
                           r
                   else
                     (visit#arg arg, [])
                 | _ -> (visit#arg arg, [])
          )
          pairs in
      let args', fins = List.split args' in
        args', List.flatten fins

    method expr expr_ =
      let p = expr_.pos in
      let e,ety = expr_.data in
        match e with
          | FnCall(f, args) ->
            let args',fins = visit#do_fn f args in
              mkpos (PostCheckedExpr(mkpos (FnCall(f, args'), ety), fins), ety)
          | _ -> super#expr expr_

    method stm stm_ =
      let p = stm_.pos in
        match stm_.data with
          | ArrayDec(v,vty,ae) ->
            let (ae',_) = ae.data in
              begin
                match ae' with
                  | ArrayNoinit _ ->
                    let dec = visit#declare_init v vty in
                      dec :: (super#stm stm_)
                  | ArrayView(lval,e,_) when visit#lval_is_uninit lval ->
                    let e' = visit#expr e in
                    let initname = "#" ^ v.data in
                    let basename = visit#lval_to_id lval in
                    let baseinitlval,base = List.assoc basename _inits in
                      _inits <- (initname, (baseinitlval, abinop Ast.Plus base e')) :: _inits;
                      let old = _arrsetting in
                        _arrsetting <- true;
                        let stm' = super#stm stm_ in
                          _arrsetting <- old;
                          stm'
                  | _ -> super#stm stm_
              end
          | Assign(lval,e) ->
            let lv',_ = lval.data in
              begin
                match lv' with
                  | ArrayEl(lv,e) when visit#lval_is_uninit lv ->
                    let (lval',fin) = visit#lval_set lval in
                    let e' = visit#expr e in
                      [mkpos Assign(lval', e')] @ fin
                  | _ -> super#stm stm_
              end
          | VoidFnCall(f, args) ->
            let args',fins = visit#do_fn f args in
              [mkpos VoidFnCall(f, args')] @ fins
          | Return e ->
            let e' = visit#expr e in
            let fins =
              List.map
                (fun x ->
                   let _,vty = Env.find_var _venv x in
                   let lexpr = Tast_utils.refvt_to_lexpr vty in
                   let id = "#" ^ x.data in
                   let lvalinit,_ = List.assoc id _inits in
                   let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
                   let l' = visit#lexpr_to_expr lexpr in
                     call_assert(binop Ast.GTE lvalinit' l')
                )
                _clobbers in
              fins @ [mkpos Return e']
          | VoidReturn ->
            let fins =
              List.map
                (fun x ->
                   let _,vty = Env.find_var _venv x in
                   let lexpr = Tast_utils.refvt_to_lexpr vty in
                   let id = "#" ^ x.data in
                   let lvalinit,_ = List.assoc id _inits in
                   let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
                   let l' = visit#lexpr_to_expr lexpr in
                     call_assert(binop Ast.GTE lvalinit' l')
                )
                _clobbers in
              fins @ [mkpos VoidReturn]
          | For(i,bty,init,cond,upd,block) as stm ->
            let do_assert =
              match cond.data with
                | BinOp(op,a,b),_ ->
                  begin
                    match op with
                      | Ast.LT
                      | Ast.LTE
                      | Ast.GT
                      | Ast.GTE ->
                        let a_is_i =
                          match a.data with
                            | Lvalue {data=(Base x,_)},_ ->
                              x.data = i.data
                            | _ -> false
                        in
                        let b_is_i =
                          match b.data with
                            | Lvalue {data=(Base y,_)},_ ->
                              y.data = i.data
                            | _ -> false
                        in
                          a_is_i || b_is_i
                      | _ -> false
                  end
                | _ -> false
            in
            let stm' =
              if do_assert then
                let BinOp(op,_,_),_ = cond.data in
                let ebool = BaseET(mkpos Bool, mkpos Fixed Public) in

                let venv,stms = block in
                let vty = mkp(i) RefVT(bty, mkp(i) Fixed Public, mkp(i) Const) in
                let ety = BaseET(bty, mkp(i) Fixed Public) in
                let nondet = mkpos (FnCall(get_nondet bty, []), ety) in
                let idec = mkpos BaseDec(i, vty, nondet) in
                let ivar = mkpos (Lvalue(mkpos (Base i, vty.data)), ety) in

                let progress = call_assert(binop op ivar upd) in
                let lobound = call_assume(binop Ast.LogicalOr
                                            (binop op init ivar)
                                            (binop Ast.Equal init ivar)) in

                let stms' = [progress; lobound] @ stms in
                let ifblock = mkpos If(cond, (venv,stms'), (venv,[])) in
                  mkpos Block(venv, [idec; ifblock])
              else stm_ in
              super#stm stm'
          | _ -> super#stm stm_

    method param param_ =
      let p = param_.pos in
      begin
        match param_.data with
          | Param(_,({data=ArrayVT _} as vty),_)
          | Param(_,({data=StructVT _} as vty),_) ->
            _paraminits <- param_ :: _paraminits
          | _ -> ()
      end;
      super#param param_

  end

let transform m =
  let visit = new smack_visitor in
    visit#fact_module m
