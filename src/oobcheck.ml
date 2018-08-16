open Util
open Pos
open Err
open Tast
open Tast_util

open Z3

let mk_eq = Boolean.mk_eq

let ctx = mk_context []
let bv = BitVector.mk_sort ctx

let zpush exprs e z =
  if mlist_mem !exprs e ~equal:(=) then
    (let ({pos=p},_) = e in
       raise @@ cerr p "NOPE");
  mlist_push (e,z) exprs
let zpop exprs e =
  mlist_find !exprs e ~equal:(=)

class oobchecker m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val _solver = Solver.mk_solver ctx None
    val _expr : (expr * Z3.Expr.expr) mlist = ref []
    val _vmap : (var_name * Z3.Expr.expr) mlist = ref []

    method _assert p zexpr =
      let negated = Boolean.mk_not ctx zexpr in
        Solver.push _solver;
        Solver.add _solver [negated];
        begin
          match (Solver.check _solver []) with
            | Solver.SATISFIABLE ->
              print_endline "sat!";
              let Some m = Solver.get_model _solver in
                print_endline (Model.to_string m);
                raise @@ err p
            | Solver.UNSATISFIABLE -> ()
            | Solver.UNKNOWN ->
              print_endline "unknown!";
              print_endline @@ Solver.get_reason_unknown _solver;
              raise @@ err p
        end;
        Solver.pop _solver 1

    method fdec fdec =
      Solver.reset _solver;
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
                Solver.add _solver [zeq];
                let divcheck = Boolean.mk_not ctx (mk_eq ctx z2 (Expr.mk_numeral_int ctx 0 bvec_sort)) in
                  visit#_assert (expr_of e2).pos divcheck;
                  (if is_signed bty
                   then mk_sdiv
                   else mk_udiv) ctx z1 z2
            | Ast.Modulo ->
              let bvec_sort = bty |> bitsize |> bv in
              let zdec = Expr.mk_fresh_const ctx "divisor" bvec_sort in
              let zeq = mk_eq ctx zdec z2 in
                Solver.add _solver [zeq];
                let divcheck = Boolean.mk_not ctx (mk_eq ctx z2 (Expr.mk_numeral_int ctx 0 bvec_sort)) in
                  visit#_assert (expr_of e2).pos divcheck;
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
                Solver.add _solver [zeq];
                let shiftcheck_bot = cmpe ctx (Expr.mk_numeral_int ctx 0 (bv e2size)) z2 in
                let shiftcheck_top = cmp ctx z2 (Expr.mk_numeral_int ctx e1size (bv e2size)) in
                let shiftcheck = Boolean.mk_and ctx [shiftcheck_bot; shiftcheck_top] in
                  visit#_assert (expr_of e2).pos shiftcheck;
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
                Solver.add _solver [zeq];
                let shiftcheck_bot = cmpe ctx (Expr.mk_numeral_int ctx 0 (bv e2size)) z2 in
                let shiftcheck_top = cmp ctx z2 (Expr.mk_numeral_int ctx e1size (bv e2size)) in
                let shiftcheck = Boolean.mk_and ctx [shiftcheck_bot; shiftcheck_top] in
                  visit#_assert (expr_of e2).pos shiftcheck;
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
                Solver.add _solver [zeq];
                let shiftcheck_bot = cmpe ctx (Expr.mk_numeral_int ctx 0 (bv e2size)) z2 in
                let shiftcheck_top = cmp ctx z2 (Expr.mk_numeral_int ctx e1size (bv e2size)) in
                let shiftcheck = Boolean.mk_and ctx [shiftcheck_bot; shiftcheck_top] in
                  visit#_assert (expr_of e2).pos shiftcheck;
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
                Solver.add _solver [zeq];
                let shiftcheck_bot = cmpe ctx (Expr.mk_numeral_int ctx 0 (bv e2size)) z2 in
                let shiftcheck_top = cmp ctx z2 (Expr.mk_numeral_int ctx e1size (bv e2size)) in
                let shiftcheck = Boolean.mk_and ctx [shiftcheck_bot; shiftcheck_top] in
                  visit#_assert (expr_of e2).pos shiftcheck;
                  mk_ext_rotate_right ctx z1 z2
        end)

    method _lexpr lexpr_ =
      let p = lexpr_.pos in
        match lexpr_.data with
          | LIntLiteral n ->
            Expr.mk_numeral_int ctx n (bv 64)
          | LDynamic x ->
            mlist_find ~equal:vequal !_vmap x
            >!!> err p

    method expr ((e_,bty_) as e__) =
      let p = e_.pos in
      let res =
        match e_.data with
          | TernOp ((_, cty) as cond,e1,e2) when (label_of cty).data = Public ->
            begin
              let cond' = visit#expr cond in
                zpop _expr cond >>= fun zcond ->
                Solver.push _solver;
                Solver.add _solver [zcond];
                let e1' = visit#expr e1 in
                  Solver.pop _solver 1;
                  Solver.push _solver;
                  Solver.add _solver [Boolean.mk_not ctx zcond];
                  let e2' = visit#expr e2 in
                    Solver.pop _solver 1;
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
            (* Non-blessable *)
            | Declassify _
            | Enref _
            | Deref _
              -> ()
            | ArrayGet (e,lexpr) ->
              begin
                let e_bty = type_of e in
                let Arr (_,e_len,_) = e_bty.data in
                let zlen = visit#_lexpr e_len in
                let zi = visit#_lexpr lexpr in
                let zdec = Expr.mk_fresh_const ctx "arrayget" (bv 64) in
                let zeq = mk_eq ctx zdec zi in
                  Solver.add _solver [zeq];
                  let boundscheck_bot = BitVector.mk_ule ctx (Expr.mk_numeral_int ctx 0 (bv 64)) zi in
                  let boundscheck_top = BitVector.mk_ult ctx zi zlen in
                  let boundscheck = Boolean.mk_and ctx [boundscheck_bot; boundscheck_top] in
                    return @@ visit#_assert lexpr.pos boundscheck
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
                  Solver.add _solver [zeq];
                  let boundscheck_bot = BitVector.mk_ule ctx (Expr.mk_numeral_int ctx 0 (bv 64)) zi in
                  let boundscheck_mid = BitVector.mk_ule ctx zi zend in
                  let boundscheck_top = BitVector.mk_ule ctx zend zlen in
                  let boundscheck = Boolean.mk_and ctx [boundscheck_bot; boundscheck_mid; boundscheck_top] in
                    return @@ visit#_assert (expr_of e).pos boundscheck
              end |> consume
            | Shuffle (_,_)
            | StructLit _
            | StructGet (_,_)
            | StringLiteral _
              -> ()
        end;
        res

    method stm (stm_,lbl_) =
      let p = stm_.pos in
      let res =
        match stm_.data with
          | VarDec (x,bty,e) ->
            let res = super#stm (stm_,lbl_) in
              (begin
                match bty.data with
                  | Bool _ ->
                    Some (Boolean.mk_const_s ctx x.data)
                  | UInt (s,_)
                  | Int  (s,_) ->
                    Some (BitVector.mk_const_s ctx x.data s)
                  | _ -> None
              end >>= fun zdec ->
               mlist_push (x,zdec) _vmap;
               zpop _expr e >>= fun ze ->
               let zassign = mk_eq ctx zdec ze in
                 return @@ Solver.add _solver [zassign]) |> consume;
              return res
          | FnCall (x,bty,fn,args) ->
            let res = super#stm (stm_,lbl_) in
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

          | If (cond,thens,elses) ->
            let cond' = visit#expr cond in
            let cty = type_of cond' in
              if (label_of cty).data = Public then
                zpop _expr cond >>= fun zcond ->
                let zncond = Boolean.mk_not ctx zcond in
                Solver.push _solver;
                Solver.add _solver [zcond];
                let thens' = visit#block thens in
                  Solver.pop _solver 1;
                  Solver.push _solver;
                  Solver.add _solver [zncond];
                  let elses' = visit#block elses in
                    Solver.pop _solver 1;
                    if ends_with_ret thens' then
                      Solver.add _solver [zncond];
                    if ends_with_ret elses' then
                      Solver.add _solver [zcond];
                    return (p@>If (cond',thens',elses'),lbl_)
              else
                (* secret ifs don't guard statements! *)
                let thens' = visit#block thens in
                let elses' = visit#block elses in
                  return (p@>If (cond',thens',elses'),lbl_)
          (* XXX need to collect join information post if-block? *)
          (* maybe not, since side-effects (refs) aren't blessable *)

          | RangeFor (x,bty,lo,hi,blk) ->
            let lo' = visit#expr lo in
            let hi' = visit#expr hi in
              zpop _expr hi' >>= fun zhi ->
              zpop _expr lo' >>= fun zlo ->
              begin
                match bty.data with
                  | UInt (s,_)
                  | Int (s,_) ->
                    Some (BitVector.mk_const_s ctx x.data s)
                  | _ -> None
              end >>= fun zdec ->
              let zconstraint =
                Boolean.mk_and ctx
                  (if is_signed bty then
                     [ BitVector.mk_sle ctx zlo zdec ;
                       BitVector.mk_slt ctx zdec zhi ]
                   else
                     [ BitVector.mk_ule ctx zlo zdec ;
                       BitVector.mk_ult ctx zdec zhi ]) in
                Solver.push _solver;
                Solver.add _solver [zconstraint];
                let blk' = visit#block blk in
                  Solver.pop _solver 1;
                  return (p@>RangeFor (x,bty,lo',hi',blk'),lbl_)
          (* XXX need to collect join information post for-block? *)
          (* maybe not, since side-effects (refs) aren't blessable *)

          | Assume e ->
            let res = super#stm (stm_,lbl_) in
              zpop _expr e >>= fun z ->
              Solver.add _solver [z];
              return res

          | Block _
          | VoidFnCall (_,_)
          | Assign (_,_) (* update info? or no since refs are not blessable? *)
          | ArrayFor (_,_,_,_) (* collect join info? *)
          | Return _
          | VoidReturn
            -> return @@ super#stm (stm_,lbl_)
      in
        res >!!> cerr p "error from oob#stm"

  end

let transform m =
  let visit = new oobchecker m in
    visit#fact_module ()
