open Util
open Pos
open Err
open Tast
open Tast_util

open Z3

let mk_eq = Boolean.mk_eq

let ctx = mk_context []
let bv = BitVector.mk_sort ctx

let zpush stack e = push (Some e) stack
let zpop stack = pop stack

class oobchecker m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val _solver = Solver.mk_solver ctx None
    val _expr : Z3.Expr.expr option stack = Stack.create ()
    val _vmap : (var_name * Z3.Expr.expr) mlist = ref []

    method _assert zexpr =
      let negated = Boolean.mk_not ctx zexpr in
        Solver.push _solver;
        Solver.add _solver [negated];
        begin
          match (Solver.check _solver []) with
            | Solver.SATISFIABLE ->
              print_endline "sat!";
              let Some m = Solver.get_model _solver in
                print_endline (Model.to_string m);
                raise @@ err fake_pos
            | Solver.UNSATISFIABLE -> ()
            | Solver.UNKNOWN ->
              print_endline "unknown!";
              print_endline @@ Solver.get_reason_unknown _solver;
              raise @@ err fake_pos
        end;
        Solver.pop _solver 1

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
        end |> ignore; res

    method binop op bty e1 e2 =
      BitVector.(
        zpop _expr >>= fun z2 ->
        zpop _expr >>= fun z1 ->
        return @@
        zpush _expr
          (match op with
            | Ast.Plus ->
              mk_add ctx z1 z2
            | Ast.Minus
            | Ast.Multiply
            | Ast.Divide
            | Ast.Modulo
            | Ast.Equal
            | Ast.NEqual
              -> raise @@ err fake_pos
            | Ast.GT ->
              let cmp =
                if is_signed ((type_of e1) +: (type_of e2))
                then mk_sgt
                else mk_ugt in
                cmp ctx z1 z2
            | Ast.GTE
              -> raise @@ err fake_pos
            | Ast.LT ->
              let cmp =
                if is_signed ((type_of e1) +: (type_of e2))
                then mk_slt
                else mk_ult in
                cmp ctx z1 z2
            | Ast.LTE
              -> raise @@ err fake_pos
            | Ast.LogicalAnd ->
              Boolean.mk_and ctx [z1; z2]
            | Ast.LogicalOr
            | Ast.BitwiseAnd
            | Ast.BitwiseOr
            | Ast.BitwiseXor
              -> raise @@ err fake_pos
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
                  visit#_assert shiftcheck;
                  mk_shl ctx z1 z2
            | Ast.RightShift
            | Ast.LeftRotate
            | Ast.RightRotate
              -> raise @@ err fake_pos)
      ) |> ignore

    method expr (e_,bty_) =
      let p = e_.pos in
      let res = super#expr (e_,bty_) in
        begin
          match e_.data with
            (* Blessable *)
            | True ->
              zpush _expr (Boolean.mk_true ctx)
            | False ->
              zpush _expr (Boolean.mk_false ctx)
            | IntLiteral n ->
              let (UInt (s,_) | Int (s,_)) = bty_.data in
                zpush _expr (Expr.mk_numeral_int ctx n (bv s))
            | Variable x ->
              begin
                Core.List.Assoc.find ~equal:vequal !_vmap x >>= fun zdec ->
                return @@ zpush _expr zdec
              end >!!> cerr p "couldn't find '%s'" x.data
            | Cast (_,_)
            | UnOp (_,_) -> (* XXX *) push None _expr
            | BinOp (op,e1,e2) ->
              visit#binop op bty_ e1 e2
            | TernOp (_,_,_)
            | Select (_,_,_)
            (* Non-blessable *)
            | Declassify _
            | Enref _
            | Deref _
            | ArrayGet (_,_)
            | ArrayLit _
            | ArrayZeros _
            | ArrayCopy _
            | ArrayView (_,_,_)
            | Shuffle (_,_)
            | StructLit _
            | StructGet (_,_)
            | StringLiteral _
              -> push None _expr
        end;
        res

    method stm (stm_,lbl_) =
      let p = stm_.pos in
      let res =
        match stm_.data with
          | VarDec (x,bty,e) ->
            let res = super#stm (stm_,lbl_) in
              begin
                match bty.data with
                  | Bool _ ->
                    Some (Boolean.mk_const_s ctx x.data)
                  | UInt (s,_)
                  | Int  (s,_) ->
                    Some (BitVector.mk_const_s ctx x.data s)
                  | _ -> None
              end >>= fun zdec ->
              mlist_push (x,zdec) _vmap;
              zpop _expr >>= fun ze ->
              let zassign = mk_eq ctx zdec ze in
                Solver.add _solver [zassign];
                return res


          | If (cond,thens,elses) ->
            let cond' = visit#expr cond in
              zpop _expr >>= fun zcond ->
              Solver.push _solver;
              Solver.add _solver [zcond];
              let thens' = visit#block thens in
                Solver.pop _solver 1;
                Solver.add _solver [Boolean.mk_not ctx zcond];
                Solver.push _solver;
                let elses' = visit#block elses in
                  Solver.pop _solver 1;
                  return (p@>If (cond',thens',elses'),lbl_)

          | Block _
          | FnCall (_,_,_,_)
          | VoidFnCall (_,_)
          | Assign (_,_)
          | RangeFor (_,_,_,_,_)
          | ArrayFor (_,_,_,_)
          | Return _
          | VoidReturn
          | Assume _
            -> return @@ super#stm (stm_,lbl_)
      in
        res >!!> err p

  end

let transform m =
  let visit = new oobchecker m in
    visit#fact_module ()
