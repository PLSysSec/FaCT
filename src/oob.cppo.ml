open Pos
open Err
open Tast
open Tast_utils
open Range

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define cwarn(msg, p) InternalCompilerError("warning: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

#define DEBUG 0
#if (DEBUG = 1)
  #define dprintf Printf.eprintf
#else
  #define dprintf Printf.ifprintf ignore
#endif

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
                dprintf "    %s : %s\n"
                  Pseudocode.(ps_lval {indent=0} lval_)
                  Pseudocode.(ps_lexpr lexpr);
                warn @@ cwarn("unchecked array access", p)
              | Error ->
                dprintf "    %s : %s\n"
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
                dprintf "    %s\n"
                  Pseudocode.(ps_aexpr {indent=0} ae_);
                warn @@ cwarn("unchecked array access", p)
              | Error ->
                dprintf "    %s\n"
                  Pseudocode.(ps_aexpr {indent=0} ae_);
                raise @@ cerr("invalid array access", p)
              | Ok -> ())
        | _ -> ());
      super#aexpr ae_

    method stm stm_ =
      let p = stm_.pos in
        (match stm_.data with
          | BaseDec(x,vty,e) ->
            (match vty.data with
              | RefVT({data=(UInt _ | Int _)}, {data=Fixed Public}, {data=Const}) ->
                let e' = Range.of_expr _ranges e in
                  if e' <> None then
                    dprintf
                      "%s <- %s\n" x.data (show_range e');
                    _ranges <- (x, e') :: _ranges
              | _ -> ())
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
              (match (of_expr _ranges init),(of_expr _ranges endlimit),(xnum step) with
                | (Some _ as init'), (Some _ as endlimit'), Some step' ->
                  let upward = ((upd_op = Ast.Plus && step' > 0) ||
                                (upd_op = Ast.Minus && step' < 0)) in
                    (match cond_op with
                      | Ast.LT ->
                        if not upward then
                          raise @@ err(p);
                        (match lt_range init' endlimit' with
                          | Some true ->
                            mk_range init' (add_range endlimit' (neg_range init'))
                          | Some false -> raise @@ err(p)
                          | _ -> None)
                      | _ -> None)
                | _ -> None) in
              (match cond_check,upd_check with
                | Some (Some cond_op, endlimit), Some (Some upd_op, step) ->
                  (match check_isrange init cond_op endlimit upd_op step with
                    | Some _ as range ->
                      dprintf
                        "%s <- %s\n" i.data (show_range range);
                      _ranges <- (i, range) :: _ranges
                    | None ->
                      (*dprintf
                        "  -->  %s  <--\n"
                        Pseudocode.(ps_stm {indent=0} (mkfake For(i,bty,init,cond,upd,(Env.new_env (),[]))));
                        warn @@ cwarn("uncool for loop", stm_.pos)*)()
                  )
                | _ ->
                  warn @@ cwarn("uncool for loop", stm_.pos)
              )
          | _ -> ());
        super#stm stm_

    method param param_ =
      (match param_.data with
        | Param(x,vty,attr) ->
          (match refvt_to_lexpr_option vty with
            | Some lexpr' ->
              (match lexpr' with
                | LDynamic x ->
                  let range = Some (X(x.data,Q.one,0), N 1) in
                  dprintf
                    "%s <- %s\n" x.data (show_range range);
                  _ranges <- (x, range) :: _ranges
                | _ -> ())
            | None -> ())
        | _ -> ());
      super#param param_

    method fdec =
      _ranges <- []; super#fdec

  end

let transform m =
  let visit = new oob_visitor in
    visit#fact_module m
