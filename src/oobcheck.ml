open Util
open Pos
open Err
open Tast
open Tast_util

module LogModule = Log
open Z3
module Log = LogModule

(** TODO:
    - if (e1) {
        if (e2) {
          ...
          return;
        } else {
          ...
          return;
        }
      }
      constraints do not propagate properly
    - if (e_always_true) {
        ...
        return;
      }
      will still be treated as a possible-return instead of an always-return
*)

let mk_eq = Boolean.mk_eq

let ctx = mk_context []
let bv = BitVector.mk_sort ctx

let zpush exprs e z =
  (*if mlist_mem !exprs e ~equal:(=) then
    (let ({pos=p},_) = e in
       raise @@ cerr p "NOPE");*)
  mlist_push (e,z) exprs
let zpop exprs e =
  mlist_find !exprs e ~equal:(=)

let print_assumptions solver =
  print_endline ">>";
  List.iter
    (Expr.to_string %> ((^) "  ") %> print_endline)
    (Solver.get_assertions solver);
  print_endline "<<"

class oobchecker debug m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val _solver = Solver.mk_solver ctx None
    val _expr : (expr * Z3.Expr.expr) mlist = ref []
    val _vmap : (var_name * Z3.Expr.expr) mlist = ref []
    val _assertion_stack : Z3.Expr.expr list stack = Stack.create ()
    val mutable _assertions_are_on : bool = true

    method _add zexpr =
      Solver.add _solver [zexpr];

    method _add_flow zexpr =
      Solver.add _solver [zexpr];
      push (zexpr :: pop _assertion_stack) _assertion_stack

    method _add_all_flow zexprs =
      Solver.add _solver zexprs;
      push (zexprs @ (pop _assertion_stack)) _assertion_stack

    method _push () =
      Solver.push _solver;
      push [] _assertion_stack

    method _pop () =
      Solver.pop _solver 1

    method _pop_with_flow () =
      Solver.pop _solver 1;
      pop _assertion_stack

    method _temporarily_turn_off_assertions () =
      _assertions_are_on <- false

    method _ok_turn_assertions_back_on () =
      _assertions_are_on <- true

    method _assert p zcheckvar zexprs =
      List.iter (visit#_assert_single p zcheckvar) zexprs
      (*let zexpr = Boolean.mk_and ctx zexprs in
        visit#_assert_single p zcheckvar zexpr*)

    method _assert_single p zcheckvar zexpr =
      if _assertions_are_on then
        begin
          let negated = Boolean.mk_not ctx zexpr in
            visit#_push ();
            visit#_add negated;
            begin
              match (Solver.check _solver []) with
                | Solver.SATISFIABLE ->
                  let Some m = Solver.get_model _solver in
                    if debug then
                      begin
                        print_endline (Model.to_string m);
                        print_assumptions _solver
                      end;
                    let message =
                      (Model.eval m zcheckvar true >>= fun thinger ->
                       let value = Expr.to_string thinger in
                         return @@ Printf.sprintf "value could be %s" value)
                      >!!> err p
                    in
                      raise @@ cerr p "%s" message
                | Solver.UNSATISFIABLE -> ()
                | Solver.UNKNOWN ->
                  print_endline "unknown!";
                  print_endline @@ Solver.get_reason_unknown _solver;
                  raise @@ err p
            end;
            visit#_pop ()
        end

    method _assert_sat p zexpr =
      visit#_push ();
      visit#_add zexpr;
      begin
        match (Solver.check _solver []) with
          | Solver.SATISFIABLE -> ()
          | Solver.UNSATISFIABLE ->
            Log.error "unsat!";
            raise @@ err p
          | Solver.UNKNOWN ->
            print_endline "unknown!";
            print_endline @@ Solver.get_reason_unknown _solver;
            raise @@ err p
      end;
      visit#_pop ()

    method fdec fdec =
      Solver.reset _solver;
      Stack.clear _assertion_stack;
      push [] _assertion_stack;
      begin match fdec.data with
        | FunDec (fn,_,_,_,_) ->
          Log.debug "oobchecking %s..." fn.data
        | _ -> ()
      end;
      super#fdec fdec

    method param param =
      let res = super#param param in
        begin
          match param.data with
            | Param (x,bty) ->
              begin
                match bty.data with
                  | Bool _ ->
                    Some (Boolean.mk_const_s ctx x.data)
                  | UInt (s,_)
                  | Int  (s,_) ->
                    Some (BitVector.mk_const_s ctx x.data s)
                  | _ -> None
              end >>= fun zdec ->
              return @@ mlist_push (x,zdec) _vmap
        end |> consume; res

    method unop op bty e =
      BitVector.(
        zpop _expr e >>= fun z ->
        return @@
        (match op with
          | Ast.Neg -> mk_neg ctx z
          | Ast.LogicalNot -> Boolean.mk_not ctx z
          | Ast.BitwiseNot -> mk_not ctx z)
      )

    method binop op bty e1 e2 =
      BitVector.(
        zpop _expr e2 >>= fun z2 ->
        zpop _expr e1 >>= fun z1 ->
        return @@
        begin
          match op with
            | Ast.Plus ->
              mk_add ctx z1 z2
            | Ast.Minus ->
              mk_sub ctx z1 z2
            | Ast.Multiply ->
              mk_mul ctx z1 z2
            | Ast.Divide ->
              let bvec_sort = bty |> bitsize |> bv in
              let zdec = Expr.mk_fresh_const ctx "divisor" bvec_sort in
              let zeq = mk_eq ctx zdec z2 in
                visit#_add zeq;
                let divcheck = Boolean.mk_not ctx (mk_eq ctx z2 (Expr.mk_numeral_int ctx 0 bvec_sort)) in
                  visit#_assert (expr_of e2).pos zdec [divcheck];
                  (if is_signed bty
                   then mk_sdiv
                   else mk_udiv) ctx z1 z2
            | Ast.Modulo ->
              let bvec_sort = bty |> bitsize |> bv in
              let zdec = Expr.mk_fresh_const ctx "divisor" bvec_sort in
              let zeq = mk_eq ctx zdec z2 in
                visit#_add zeq;
                let divcheck = Boolean.mk_not ctx (mk_eq ctx z2 (Expr.mk_numeral_int ctx 0 bvec_sort)) in
                  visit#_assert (expr_of e2).pos zdec [divcheck];
                  (if is_signed bty
                   then mk_srem
                   else mk_urem) ctx z1 z2
            | Ast.Equal ->
              mk_eq ctx z1 z2
            | Ast.NEqual ->
              Boolean.mk_not ctx @@ mk_eq ctx z1 z2
            | Ast.GT ->
              let cmp =
                if is_signed ((type_of e1) +: (type_of e2))
                then mk_sgt
                else mk_ugt in
                cmp ctx z1 z2
            | Ast.GTE ->
              let cmp =
                if is_signed ((type_of e1) +: (type_of e2))
                then mk_sge
                else mk_uge in
                cmp ctx z1 z2
            | Ast.LT ->
              let cmp =
                if is_signed ((type_of e1) +: (type_of e2))
                then mk_slt
                else mk_ult in
                cmp ctx z1 z2
            | Ast.LTE ->
              let cmp =
                if is_signed ((type_of e1) +: (type_of e2))
                then mk_sle
                else mk_ule in
                cmp ctx z1 z2
            | Ast.LogicalAnd ->
              Boolean.mk_and ctx [z1; z2]
            | Ast.LogicalOr ->
              Boolean.mk_or ctx [z1; z2]
            | Ast.BitwiseAnd ->
              mk_and ctx z1 z2
            | Ast.BitwiseOr ->
              mk_or ctx z1 z2
            | Ast.BitwiseXor ->
              mk_xor ctx z1 z2
            | Ast.LeftShift ->
              let cmp,cmpe =
                if is_signed (type_of e2)
                then mk_slt,mk_sle
                else mk_ult,mk_ule in
              let e1size = bitsize (type_of e1) in
              let e2size = bitsize (type_of e2) in
              let zdec = Expr.mk_fresh_const ctx "shiftamt" (bv @@ bitsize @@ type_of e2) in
              let zeq = mk_eq ctx zdec z2 in
                visit#_add zeq;
                let shiftcheck_bot = cmpe ctx (Expr.mk_numeral_int ctx 0 (bv e2size)) z2 in
                let shiftcheck_top = cmp ctx z2 (Expr.mk_numeral_int ctx e1size (bv e2size)) in
                  visit#_assert (expr_of e2).pos zdec [shiftcheck_bot; shiftcheck_top];
                  mk_shl ctx z1 z2
            | Ast.RightShift ->
              let cmp,cmpe =
                if is_signed (type_of e2)
                then mk_slt,mk_sle
                else mk_ult,mk_ule in
              let e1size = bitsize (type_of e1) in
              let e2size = bitsize (type_of e2) in
              let zdec = Expr.mk_fresh_const ctx "shiftamt" (bv @@ bitsize @@ type_of e2) in
              let zeq = mk_eq ctx zdec z2 in
                visit#_add zeq;
                let shiftcheck_bot = cmpe ctx (Expr.mk_numeral_int ctx 0 (bv e2size)) z2 in
                let shiftcheck_top = cmp ctx z2 (Expr.mk_numeral_int ctx e1size (bv e2size)) in
                  visit#_assert (expr_of e2).pos zdec [shiftcheck_bot; shiftcheck_top];
                  (if is_signed (type_of e1)
                   then mk_ashr
                   else mk_lshr)
                    ctx z1 z2
            | Ast.LeftRotate ->
              let cmp,cmpe =
                if is_signed (type_of e2)
                then mk_slt,mk_sle
                else mk_ult,mk_ule in
              let e1size = bitsize (type_of e1) in
              let e2size = bitsize (type_of e2) in
              let zdec = Expr.mk_fresh_const ctx "shiftamt" (bv @@ bitsize @@ type_of e2) in
              let zeq = mk_eq ctx zdec z2 in
                visit#_add zeq;
                let shiftcheck_bot = cmpe ctx (Expr.mk_numeral_int ctx 0 (bv e2size)) z2 in
                let shiftcheck_top = cmp ctx z2 (Expr.mk_numeral_int ctx e1size (bv e2size)) in
                  visit#_assert (expr_of e2).pos zdec [shiftcheck_bot; shiftcheck_top];
                  mk_ext_rotate_left ctx z1 z2
            | Ast.RightRotate ->
              let cmp,cmpe =
                if is_signed (type_of e2)
                then mk_slt,mk_sle
                else mk_ult,mk_ule in
              let e1size = bitsize (type_of e1) in
              let e2size = bitsize (type_of e2) in
              let zdec = Expr.mk_fresh_const ctx "shiftamt" (bv @@ bitsize @@ type_of e2) in
              let zeq = mk_eq ctx zdec z2 in
                visit#_add zeq;
                let shiftcheck_bot = cmpe ctx (Expr.mk_numeral_int ctx 0 (bv e2size)) z2 in
                let shiftcheck_top = cmp ctx z2 (Expr.mk_numeral_int ctx e1size (bv e2size)) in
                  visit#_assert (expr_of e2).pos zdec [shiftcheck_bot; shiftcheck_top];
                  mk_ext_rotate_right ctx z1 z2
        end)

    method _lexpr lexpr_ =
      let p = lexpr_.pos in
        match lexpr_.data with
          | LIntLiteral n ->
            Expr.mk_numeral_int ctx n (bv 64)
          | LDynamic x ->
            mlist_find ~equal:vequal !_vmap x
            >!!> cerr p "couldn't find '%s'" x.data

    method expr ((e_,bty_) as e__) =
      let p = e_.pos in
      let res =
        match e_.data with
          | TernOp ((_, cty) as cond,e1,e2) when (label_of cty).data = Public ->
            begin
              let cond' = visit#expr cond in
                zpop _expr cond >>= fun zcond ->
                visit#_push ();
                visit#_add zcond;
                let e1' = visit#expr e1 in
                  visit#_pop ();
                  visit#_push ();
                  visit#_add (Boolean.mk_not ctx zcond);
                  let e2' = visit#expr e2 in
                    visit#_pop ();
                    return @@ (p@>TernOp (cond',e1',e2'),bty_)
            end >!!> err p
          | _ -> super#expr (e_,bty_) in
        begin
          match e_.data with
            (* Blessable *)
            | True ->
              zpush _expr e__ (Boolean.mk_true ctx)
            | False ->
              zpush _expr e__ (Boolean.mk_false ctx)
            | IntLiteral n ->
              let (UInt (s,_) | Int (s,_)) = bty_.data in
                zpush _expr e__ (Expr.mk_numeral_int ctx n (bv s))
            | Variable x ->
              begin
                mlist_find ~equal:vequal !_vmap x >>= fun zdec ->
                return @@ zpush _expr e__ zdec
              end |> consume
            | Cast (bty,e) ->
              let e_bty = type_of e in
              let old_size = bitsize e_bty in
              let new_size = bitsize bty in
                begin
                  zpop _expr e >>= fun z ->
                  let casted =
                    match compare old_size new_size with
                      | res when res < 0 ->
                        (if is_signed e_bty
                         then BitVector.mk_sign_ext
                         else BitVector.mk_zero_ext)
                          ctx (new_size - old_size) z
                      | res when res > 0 ->
                        BitVector.mk_extract ctx (new_size - 1) 0 z
                      | _ -> z in
                    return @@ zpush _expr e__ casted
                end |> consume
            | UnOp (op,e) ->
              begin
                visit#unop op bty_ e >>= fun zexpr ->
                return @@ zpush _expr e__ zexpr
              end |> consume
            | BinOp (op,e1,e2) ->
              begin
                visit#binop op bty_ e1 e2 >>= fun zexpr ->
                return @@ zpush _expr e__ zexpr
              end |> consume
            | TernOp (cond,e1,e2)
            | Select (cond,e1,e2) ->
              begin
                zpop _expr e2 >>= fun z2 ->
                zpop _expr e1 >>= fun z1 ->
                zpop _expr cond >>= fun zcond ->
                let zexpr = Boolean.mk_ite ctx zcond z1 z2 in
                  return @@ zpush _expr e__ zexpr
              end |> consume
            | Declassify e
            | Classify e ->
              begin
                zpop _expr e >>= fun z ->
                return @@ zpush _expr e__ z
              end |> consume
            (* Non-blessable *)
            | Enref _ -> ()
            | Deref ({data=Variable x},_) ->
              begin
                mlist_find ~equal:vequal !_vmap x >>= fun zdec ->
                return @@ zpush _expr e__ zdec
              end |> consume
            | Deref _ -> ()
            | ArrayGet (e,lexpr) ->
              begin
                let e_bty = type_of e in
                let Arr (_,e_len,_) = e_bty.data in
                let zlen = visit#_lexpr e_len in
                let zi = visit#_lexpr lexpr in
                let zdec = Expr.mk_fresh_const ctx "arrayget" (bv 64) in
                let zeq = mk_eq ctx zdec zi in
                  visit#_add zeq;
                  let boundscheck_bot = BitVector.mk_ule ctx (Expr.mk_numeral_int ctx 0 (bv 64)) zi in
                  let boundscheck_top = BitVector.mk_ult ctx zi zlen in
                    return @@ visit#_assert lexpr.pos zdec [boundscheck_bot; boundscheck_top]
              end |> consume
            | ArrayLit _
            | ArrayZeros _
            | ArrayCopy _
              -> ()
            | ArrayView (e,start,new_len) ->
              begin
                let e_bty = type_of e in
                let Arr (_,e_len,_) = e_bty.data in
                let zlen = visit#_lexpr e_len in
                let zi = visit#_lexpr start in
                let znewlen = visit#_lexpr new_len in
                let zend = BitVector.mk_add ctx zi znewlen in
                let zdec = Expr.mk_fresh_const ctx "arrayview" (bv 64) in
                let zeq = mk_eq ctx zdec zend in
                  visit#_add zeq;
                  let boundscheck_bot = BitVector.mk_ule ctx (Expr.mk_numeral_int ctx 0 (bv 64)) zi in
                  let boundscheck_mid = BitVector.mk_ule ctx zi zend in
                  let boundscheck_top = BitVector.mk_ule ctx zend zlen in
                    return @@ visit#_assert (expr_of e).pos zend [boundscheck_bot; boundscheck_mid; boundscheck_top]
              end |> consume
            | VectorLit _
            | Shuffle (_,_)
            | StructLit _
            | StructGet (_,_)
            | StringLiteral _
              -> ()
        end;
        res

    method block (blk_,next) =
      let p = blk_.pos in
      let next' () = (visit#next next) in
      let res =
        match blk_.data with
          | If (cond,thens,elses) ->
            let cond' = visit#expr cond in
            let cty = type_of cond' in
              if (label_of cty).data = Public then
                match zpop _expr cond with
                  | Some zcond ->
                    let zncond = Boolean.mk_not ctx zcond in
                      visit#_push ();
                      visit#_add zcond;
                      let thens' = visit#block thens in
                        visit#_pop ();
                        visit#_push ();
                        visit#_add zncond;
                        let elses' = visit#block elses in
                          visit#_pop ();
                          if ends_with_ret thens' then
                            visit#_add_flow zncond;
                          if ends_with_ret elses' then
                            visit#_add_flow zcond;
                          return (p@>If (cond',thens',elses'), next' ())
                  | None ->
                    warn @@ werr p "couldn't infer condition";
                    let thens' = visit#block thens in
                    let elses' = visit#block elses in
                      return (p@>If (cond',thens',elses'), next' ())
              else
                (* secret ifs don't guard statements! *)
                let thens' = visit#block thens in
                let elses' = visit#block elses in
                  return (p@>If (cond',thens',elses'), next' ())

          | RangeFor (x,bty,lo,hi,blk) ->
            let lo' = visit#expr lo in
            let hi' = visit#expr hi in
            let thinger =
              begin
                match bty.data with
                  | UInt (s,_)
                  | Int (s,_) ->
                    Some (BitVector.mk_const_s ctx x.data s)
                  | _ -> None
              end >>= fun zdec ->
              mlist_push (x,zdec) _vmap;
              zpop _expr hi' >>= fun zhi ->
              zpop _expr lo' >>= fun zlo ->
              let cmp,cmpe =
                BitVector.(if is_signed bty
                           then mk_slt,mk_sle
                           else mk_ult,mk_ule) in
              let nonvacuous_loop_check = cmp ctx zlo zhi in
                visit#_assert_sat p nonvacuous_loop_check;
                let zconstraint =
                  Boolean.mk_and ctx
                    [ cmpe ctx zlo zdec ;
                      cmp  ctx zdec zhi ] in
                  visit#_push ();
                  visit#_add zconstraint;
                  visit#_push ();
                  visit#_temporarily_turn_off_assertions ();
                  let _ = visit#block blk in
                    visit#_ok_turn_assertions_back_on ();
                    let new_assumptions = visit#_pop_with_flow () in
                    let after_first_iteration = cmp ctx zlo zdec in
                    let assumptions_for_next_loop_iteration =
                      List.map
                        (fun ze ->
                           let substituted =
                             Expr.substitute_one ze
                               zdec
                               (BitVector.mk_sub ctx
                                  zdec
                                  (Expr.mk_numeral_int ctx 1 (bv @@ bitsize bty))) in
                             Boolean.mk_implies ctx
                               after_first_iteration
                               substituted
                        )
                        new_assumptions in
                      visit#_add_all_flow assumptions_for_next_loop_iteration;
                      let blk' = visit#block blk in
                        visit#_pop ();
                        return (p@>RangeFor (x,bty,lo',hi',blk'), next' ())
            in
              begin match thinger with
                | Some thing -> thinger
                | None ->
                  warn @@ werr p "couldn't infer loop invariants";
                  let blk' = visit#block blk in
                    return (p@>RangeFor (x,bty,lo',hi',blk'), next' ())
              end

          | Scope _
          | ListOfStuff _
          | ArrayFor (_,_,_,_) -> return @@ super#block (blk_,next)
      in
        res >!!> cerr p "error from oob#block"

    method stm stm_ =
      let p = stm_.pos in
      let res =
        match stm_.data with
          | VarDec (x,bty,e) ->
            let res = super#stm stm_ in
              (begin
                match bty.data with
                  | Bool _ ->
                    Some (Boolean.mk_const_s ctx x.data)
                  | UInt (s,_)
                  | Int  (s,_) ->
                    Some (BitVector.mk_const_s ctx x.data s)
                  | Ref ({data=(UInt (s,_) | Int (s,_))},_) ->
                    Some (BitVector.mk_const_s ctx x.data s)
                  | _ -> None
              end >>= fun zdec ->
               mlist_push (x,zdec) _vmap;
               zpop _expr e >>= fun ze ->
               let zassign = mk_eq ctx zdec ze in
                 return @@ visit#_add zassign) |> consume;
              return res
          | FnCall (x,bty,fn,args) ->
            let res = super#stm stm_ in
              (begin
                match bty.data with
                  | Bool _ ->
                    Some (Boolean.mk_const_s ctx x.data)
                  | UInt (s,_)
                  | Int (s,_) ->
                    Some (BitVector.mk_const_s ctx x.data s)
                  | _ -> None
              end >>= fun zdec ->
               return @@ mlist_push (x,zdec) _vmap) |> consume;
              return res

          | Assume e ->
            let res = super#stm stm_ in
              zpop _expr e >>= fun z ->
              visit#_add z;
              return res

          | VoidFnCall (_,_)
          | Assign (_,_) (* update info? or no since refs are not blessable? *)
          | Cmov (_,_,_) (* update info? or no since refs are not blessable? *)
            -> return @@ super#stm stm_
      in
        res >!!> cerr p "error from oob#stm"

  end

let transform debug m =
  let visit = new oobchecker debug m in
    visit#fact_module ()
