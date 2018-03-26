open Tast
open Pos
open Debugfun

(*
  This module transforms the AST to remove debug functions for production mode.
*)

let rec xf_stm = function
  | {data=DebugVoidFnCall(_,stms)} -> []
  | {data=If(cond,then',else'); pos=p} ->
    let venv,stms = xf_block then' in
    let venv',stms' = xf_block else' in
    let stms'' = List.flatten stms in
    let stms''' = List.flatten stms' in
    [{data=If(cond,(venv,stms''), (venv',stms''')); pos=p}]
  | {data=For(n,bt,init,cond,upexpr,b); pos=p} ->
    let venv,stms = xf_block b in
    let stms' = List.flatten stms in
    [{data=For(n,bt,init,cond,upexpr,(venv,stms')); pos=p}]
  | {data=Block b; pos=p} ->
    let venv,stms = xf_block b in
    let stms' = List.flatten stms in
    [{data=Block(venv,stms'); pos=p}]
  | stm -> [stm]

and xf_block (venv,stms) = venv, List.map xf_stm stms

let xf_fdec mode = function
  | FunDec(fn,ft,rt,params,block) ->
    let venv,stms = xf_block block in
    let stms' = List.flatten stms in
    FunDec(fn,ft,rt,params,(venv,stms'))
  | fd -> fd

let xf_module mode mod' =
  match mode, mod' with
  | DEV, _ -> mod'
  | PROD, Module(env,fdecs,sdecs) ->
    Module(env, List.map
                  (fun {data=fdec; pos=p} ->
                     {data=xf_fdec mode fdec; pos=p}) fdecs,
           sdecs)