open Pos
open Err
open Tast
open Tast_utils
open Range

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define cwarn(msg, p) InternalCompilerError("warning: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

#define mkpos make_ast p @@
#define mkp(pa) make_ast pa.pos @@
#define mkfake make_ast fake_pos @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function


class oob_visitor =
  object (visit)
    inherit Astmap.ast_visitor as super
    val mutable _ranges : (var_name * Range.t) list = []

    method lval ({data=(lval',_); pos=p} as lval_) =
      (match lval' with
        | ArrayEl(lval, e) ->
          let _,vty = lval.data in
          let lexpr = refvt_to_lexpr (mkpos vty) in
          let m = Range.of_lexpr lexpr in
          let n = Range.of_expr _ranges e in
          let res = check_contained n m in
            (match res with
              | Warn ->
                Printf.eprintf "    %s : %s\n"
                  Pseudocode.(ps_lval {indent=0} lval_)
                  Pseudocode.(ps_lexpr lexpr);
                warn @@ cwarn("unchecked array access", p)
              | Error ->
                Printf.eprintf "    %s : %s\n"
                  Pseudocode.(ps_lval {indent=0} lval_)
                  Pseudocode.(ps_lexpr lexpr);
                raise @@ cerr("invalid array access", p)
              | Ok -> ())
        | _ -> ());
      super#lval lval_

    method aexpr ({data=(ae',_); pos=p} as ae_) =
      (match ae' with
        | ArrayView(lval, e, nlexpr) ->
          let _,vty = lval.data in
          let lexpr = refvt_to_lexpr (mkpos vty) in
          let m = Range.of_lexpr lexpr in
          let n' = Range.of_lexpr nlexpr in
          let off = Range.of_expr _ranges e in
          let n = add_range n' off in
          let res = check_contained n m in
            (match res with
              | Warn ->
                Printf.eprintf "    %s\n"
                  Pseudocode.(ps_aexpr {indent=0} ae_);
                warn @@ cwarn("unchecked array access", p)
              | Error ->
                Printf.eprintf "    %s\n"
                  Pseudocode.(ps_aexpr {indent=0} ae_);
                raise @@ cerr("invalid array access", p)
              | Ok -> ())
        | _ -> ());
      super#aexpr ae_

    method stm stm_ =
      (match stm_.data with
        | For(i,bty,init,cond,upd,_) ->
          let xbase e =
            match e.data with
              | Lvalue {data=(Base x,_)},_ ->
                Some x
              | _ -> None in
          let flip = function
            | Ast.LT  -> Some Ast.GT
            | Ast.LTE -> Some Ast.GTE
            | Ast.GT  -> Some Ast.LT
            | Ast.GTE -> Some Ast.LTE
            | Ast.Equal
            | Ast.NEqual
            | Ast.Plus
            | Ast.Multiply as op -> Some op
            | _ -> None
          in
          let xop op a b =
            (match (xbase a, xbase b) with
              | Some x,_ when x.data = i.data ->
                Some (Some op, b)
              | _,Some y when y.data = i.data ->
                Some (flip op, a)
              | _ -> None)
          in
          let xnum e =
            (match e.data with
              | IntLiteral n,_ -> Some n
              | _ -> None) in
          let cond_check =
            (match cond.data with
              | BinOp(op,a,b),_ ->
                (match op with
                  | Ast.LT
                  | Ast.LTE
                  | Ast.GT
                  | Ast.GTE -> xop op a b
                  | _ -> None)
              | _ -> None) in
          let upd_check =
            (match upd.data with
              | BinOp(op,a,b),_ ->
                (match op with
                  | Ast.Plus
                  | Ast.Minus -> xop op a b
                  | _ -> None)
              | _ -> None) in
          let check_isrange init cond_op endlimit upd_op step =
            (match (xnum init),(xnum endlimit),(xnum step) with
              | Some init', Some endlimit', Some step' ->
                let upward = ((upd_op = Ast.Plus && step' > 0) ||
                              (upd_op = Ast.Minus && step' < 0)) in
                  (match cond_op with
                    | Ast.LT ->
                      if init' < endlimit' && upward
                      then Some (init', endlimit' - init')
                      else None
                    | Ast.LTE ->
                      if init' <= endlimit' && upward
                      then Some (init', endlimit' - init' + 1)
                      else None
                    | Ast.GT ->
                      if init' > endlimit' && upward
                      then Some (endlimit' + 1, init' - endlimit')
                      else None
                    | Ast.GTE ->
                      if init' >= endlimit' && upward
                      then Some (endlimit', init' - endlimit' + 1)
                      else None
                    | _ -> None)
              | _ -> None) in
            (match cond_check,upd_check with
              | Some (Some cond_op, endlimit), Some (Some upd_op, step) ->
                (match check_isrange init cond_op endlimit upd_op step with
                  | Some (n, l) ->
                    _ranges <- (i, Some (N n, N l)) :: _ranges
                  | None ->
                    (*Printf.eprintf
                      "  -->  %s  <--\n"
                      Pseudocode.(ps_stm {indent=0} (mkfake For(i,bty,init,cond,upd,(Env.new_env (),[]))));
                    warn @@ cwarn("uncool for loop", stm_.pos)*)()
                )
              | _ ->
                warn @@ cwarn("uncool for loop", stm_.pos)
            )
        | _ -> ());
      super#stm stm_

  end

let transform m =
  let visit = new oob_visitor in
    visit#fact_module m
