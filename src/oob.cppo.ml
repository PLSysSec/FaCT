open Pos
open Err
open Tast
open Tast_utils

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define cwarn(msg, p) InternalCompilerError("warning: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

let fake_pos = { file=""; line=0; lpos=0; rpos=0 }

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

    method lval ({data=(lval',_); pos=p} as lval_) =
      (match lval' with
        | ArrayEl(lval, e) ->
          let _,vty = lval.data in
          let lexpr = refvt_to_lexpr (mkpos vty) in
            (match e.data with
              | IntLiteral n,_ ->
                (match lexpr.data with
                  | LIntLiteral m ->
                    if n >= m then
                      (Printf.eprintf "  -->  %s : %s  <--\n"
                         Pseudocode.(ps_lval {indent=0} lval_)
                         Pseudocode.(ps_lexpr lexpr);
                       raise @@ cerr("invalid array access", p))
                  | _ ->
                    Printf.eprintf "  -->  %s : %s  <--\n"
                      Pseudocode.(ps_lval {indent=0} lval_)
                      Pseudocode.(ps_lexpr lexpr);
                    warn @@ cwarn("unchecked array access", p))
              | _ ->
                Printf.eprintf "  -->  %s : %s  <--\n"
                  Pseudocode.(ps_lval {indent=0} lval_)
                  Pseudocode.(ps_lexpr lexpr);
                warn @@ cwarn("unchecked array access", p))
        | _ -> ());
      super#lval lval_

  end

let transform m =
  let visit = new oob_visitor in
    visit#fact_module m
