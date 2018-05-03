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

let is_uninit {data=(ae,_)} =
  match ae with
    | ArrayNoinit _ -> true
    | _ -> false

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
          | StructEl(_lval,field) ->
            visit#lval_to_id _lval ^ "#" ^ field.data

    method lval_get_init lval =
      let id = visit#lval_to_id lval in
        List.assoc id _inits

    method declare_init x vty is_uninit =
      let lvalinit = "#" ^ x.data in
      let bvty = mkfake RefVT(mkfake Int 64, mkfake Fixed Public, mkfake Const) in
      let lvalinitlval = (mkfake (Base (mkfake lvalinit), bvty.data)) in
        _inits <- (lvalinit, lvalinitlval) :: _inits;
        let init_n =
          if is_uninit
          then mkfake Tast_utils.make_nlit fake_pos 0
          else
            let lexpr = Tast_utils.refvt_to_lexpr vty in
              visit#lexpr_to_expr lexpr in
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
      let lvalinit = visit#lval_get_init lval in
      let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
        [ call_assert(binop Ast.GTE lvalinit'
                        (abinop Ast.Plus n l')) ]

    method check_arrset lval n =
      let lvalinit = visit#lval_get_init lval in
      let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
        [ call_assert(binop Ast.GTE lvalinit' n) ]

    method finish_arrset lval n l =
      let l' = visit#lexpr_to_expr l in
      let lvalinit = visit#lval_get_init lval in
      let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
      let add = abinop Ast.Plus n l' in
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

    method block ((venv,stms) as block) =
      _venv <- venv;
      let decinits =
        List.flatten
          (List.map
             (fun param ->
                match param.data with
                  | Param(x,({data=ArrayVT _} as vty),attr) ->
                    if attr.output_only then
                      _clobbers <- x :: _clobbers;
                    [visit#declare_init x vty attr.output_only]
                  | Param(sx,({data=StructVT(sname,_)} as vty),attr) ->
                    let Struct(_,fields) = (Tast_utils.find_struct _sdecs sname).data in
                    let arrfields =
                      List.filter
                        (fun fld ->
                           match fld.data with
                             | Field(x,({data=ArrayVT _} as vty),_) -> true
                             | _ -> false)
                        fields in
                      List.map
                        (fun fld ->
                           match fld.data with
                             | Field(x,({data=ArrayVT _} as vty),_) ->
                               let x' = mkfake sx.data ^ "#" ^ x.data in
                                 if attr.output_only then
                                   _clobbers <- x :: _clobbers;
                                 visit#declare_init x' vty attr.output_only)
                        arrfields
             )
             _paraminits) in
        _paraminits <- [];
        let stms' =
          decinits @ stms in
          super#block (venv,stms')

    method lval ({data=(lval,vty); pos=p} as lval_) =
      match lval with
        | ArrayEl(lval,e) ->
          let e' = make_signed e in
            mkpos (CheckedLval(visit#check_arrget lval e' one, super#lval lval_), vty)
        | _ -> super#lval lval_

    method lval_set ({data=(lval,vty); pos=p} as lval_) =
      match lval with
        | ArrayEl(lval,e) ->
          let e' = make_signed e in
            (mkpos (CheckedLval(visit#check_arrset lval e', super#lval lval_), vty),
             visit#finish_arrset lval e' one)
        | _ -> (super#lval lval_, [])

    method aexpr ({data=(ae,ety); pos=p} as ae_) =
      match ae with
        | ArrayVar lval ->
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
        | ArrayView(lval,e,lexpr) ->
          let e' = make_signed e in
          let check = if _arrsetting then
              (_fins <- visit#finish_arrset lval e' lexpr;
               visit#check_arrset lval e')
            else
              visit#check_arrget lval e' lexpr
          in
            mkpos (CheckedArrayExpr(check, super#aexpr ae_), ety)
        | _ -> super#aexpr ae_

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
                         (arg', _fins)
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
            let dec = visit#declare_init v vty (is_uninit ae) in
              dec :: (super#stm stm_)
          | Assign(lval,e) ->
            let (lval',fin) = visit#lval_set lval in
            let e' = visit#expr e in
              [mkpos Assign(lval', e')] @ fin
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
                   let lvalinit = List.assoc id _inits in
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
                   let lvalinit = List.assoc id _inits in
                   let lvalinit' = mkfake (Lvalue lvalinit, int64ety) in
                   let l' = visit#lexpr_to_expr lexpr in
                     call_assert(binop Ast.GTE lvalinit' l')
                )
                _clobbers in
              fins @ [mkpos VoidReturn]
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
