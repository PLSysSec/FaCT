open Tast
open Pos
open Debugfun

(*
  

*)

let xf_stm = function
  | {data=DebugVoidFnCall(_,stms)} as stm -> []
  | stm -> [stm]

let xf_block (venv,stms) = venv, List.map xf_stm stms

let xf_fdec mode = function
  | FunDec(fn,ft,rt,params,block) ->
    let venv,stms = xf_block block in
    let stms' = List.flatten stms in
    FunDec(fn,ft,rt,params,(venv,stms'))
  | fd -> fd

let xf_module mode mod' =
  match mode, mod' with
  | DEV, _ -> mod'
  | PROD, Module(env,fdecs) ->
    Module(env, List.map
                (fun {data=fdec; pos=p} ->
                 {data=xf_fdec mode fdec; pos=p}) fdecs)
