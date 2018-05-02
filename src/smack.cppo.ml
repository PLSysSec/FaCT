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

let nondet_32 = "__VERIFIER_nondet_int"

let get_nondet = pfunction
  | Int 8   -> "__VERIFIER_nondet_signed_char"
  | Int 16  -> "__VERIFIER_nondet_signed_short"
  | Int 32  -> "__VERIFIER_nondet_signed_int"
  | Int 64  -> "__VERIFIER_nondet_signed_long_long"
  | UInt 8  -> "__VERIFIER_nondet_unsigned_char"
  | UInt 16 -> "__VERIFIER_nondet_unsigned_short"
  | UInt 32 -> "__VERIFIER_nondet_unsigned_int"
  | UInt 64 -> "__VERIFIER_nondet_unsigned_long_long"

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

class smack_visitor =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _venv = Env.new_env () (* hacky *)

    method lexpr_to_expr lexpr =
      match lexpr.data with
        | LIntLiteral n ->
          (* assuming x64 architecture *)
          mkfake (IntLiteral n, BaseET(mkfake Int 64, mkfake Fixed Public))
        | LDynamic len ->
          let _,len_vty = Env.find_var _venv len in
          let RefVT(bty,ml,_) = len_vty.data in
          let len_ety = BaseET(bty, mkfake (Fixed Public)) in
          let base = mkfake (Base len, len_vty.data) in
          let expr = mkfake (Lvalue base, len_ety) in
          (* assuming x64 architecture *)
          let bty = mkfake (Int 64) in
            mkfake (IntCast(bty, expr), BaseET(bty, mkfake (Fixed Public)))

    method check_arrbounds lval e =
      let _,vt = lval.data in
      let lexpr = Tast_utils.refvt_to_lexpr (make_ast fake_pos vt) in
      let len = visit#lexpr_to_expr lexpr in
      let zero = mkfake (IntLiteral 0, BaseET(mkfake Int 64, mkfake (Fixed Public))) in
        [ call_assert(binop Ast.GTE e zero) ;
          call_assert(binop Ast.LT  e len)  ]


    method fact_module m =
      let Module(fenv,fdecs,sdecs) = m in
      let p = fake_pos in (* for mkpos macro *)
      let i32param = mkpos Param(mkpos "predicate", mkpos RefVT(mkpos Int 32, mkpos Fixed Public, mkpos Const), default_param_attr) in

      let fdecs' = fdecs in

      let fdec = mkpos CExtern(mkpos smack_assume, None, [i32param]) in
      let fdecs' = fdec::fdecs' in
        Env.add_var fenv (mkpos smack_assume) (fdec, ref false);

      let fdec = mkpos CExtern(mkpos smack_assert, None, [i32param]) in
      let fdecs' = fdec::fdecs' in
        Env.add_var fenv (mkpos smack_assert) (fdec, ref false);

        let fdecs' = List.fold_left
                       (fun fdecs ty ->
                          let nondet = get_nondet (mkpos ty) in
                          let rty = mkpos BaseET(mkpos ty, mkpos Fixed Public) in
                          let fdec = mkpos CExtern(nondet, Some rty, []) in
                          let fdecs' = fdec::fdecs in
                            Env.add_var fenv nondet (fdec, ref false);
                            fdecs')
                       fdecs'
                       [ UInt 64; UInt 32; UInt 16; UInt 8;
                         Int 64;  Int 32;  Int 16;  Int 8; ] in

      let m' = Module(fenv, fdecs', sdecs) in
        super#fact_module m'

    method block ((venv,_) as block) =
      _venv <- venv;
      super#block block

    method lval ({data=(lval,vty); pos=p} as lval_) =
      match lval with
        | ArrayEl(lval,e) ->
          let e' = make_signed e in
            mkpos (CheckedLval(visit#check_arrbounds lval e', super#lval lval_), vty)
        | _ -> super#lval lval_

    method aexpr ({data=(ae,ety); pos=p} as ae_) =
      match ae with
        | ArrayView(lval,e,lexpr) ->
          let e' = make_signed e in
          let _,ety = e'.data in
          let len = visit#lexpr_to_expr lexpr in
          (* addlen = e' + len *)
          let addlen = mkpos (BinOp(Ast.Plus, e, len), ety) in
          (* top = e' + len - 1 *)
          let top = mkpos (BinOp(Ast.Minus, addlen, mkpos (IntLiteral 1, ety)), ety) in
          (* sensible: assert(e' + len > e') *)
          let sensible = call_assert(binop Ast.GT addlen e') in
          let stms =
            sensible
            :: ((visit#check_arrbounds lval e')
                @ (visit#check_arrbounds lval top)) in
            mkpos (CheckedArrayExpr(stms, super#aexpr ae_), ety)
        | _ -> super#aexpr ae_

    method expr expr_ =
      let p = expr_.pos in
      let e,ety = expr_.data in
        match e with
          | BinOp(op, e1, e2) ->
            begin
              match op with
                | LeftShift
                | RightShift
                | LeftRotate
                | RightRotate ->
                  let (_,BaseET(bty,_)) = e1.data in
                  let bitlen = mkpos (IntLiteral (Tast_utils.numbits bty), BaseET(mkpos Int 64, mkpos Fixed Public)) in
                  let e2cast = mkpos (IntCast(mkpos Int 64, e2), BaseET(mkpos Int 64, mkpos Fixed Public)) in
                  let stms = [call_assert(binop Ast.LT e2cast bitlen)] in
                    mkpos (CheckedExpr(stms, super#expr expr_), ety)
                | Divide
                | Modulo ->
                  let (_,BaseET(bty,_)) = e1.data in
                  let zero = mkpos (IntLiteral 0, BaseET(mkpos Int 64, mkpos Fixed Public)) in
                  let e2cast = mkpos (IntCast(mkpos Int 64, e2), BaseET(mkpos Int 64, mkpos Fixed Public)) in
                  let stms = [call_assert(binop Ast.NEqual e2cast zero)] in
                    mkpos (CheckedExpr(stms, super#expr expr_), ety)
                | _ -> super#expr expr_
            end
          | _ -> super#expr expr_

    method stm stm_ =
      let p = stm_.pos in
      let stm' =
        match stm_.data with
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
              else
                stm_
          | _ -> stm_
      in
        super#stm stm'
  end

let transform m =
  let visit = new smack_visitor in
    visit#fact_module m
