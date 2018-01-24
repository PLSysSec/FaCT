open Tast
open Pos

(*
  A function in Fact cannot accept a value with an array type. The reason being
  that Fact supports arrays of dynamic and static size. Dynamic size arrays are
  represented as pointers. If a dynamic size array is passed to a function that
  accepts a static size array, then LLVM is unable to cast appropriately.

  This transform converts each static size array param to a dynamic one. This is
  safe to do as long as our bounds checks are done prior to the transform.

*)

let xf_param = function
| Param(vn,
    {data=ArrayVT(
      {data=ArrayAT(bt,{data=LIntLiteral(n); pos=p3}); pos=p2},ml,mt); pos=p1}) ->
  Param(vn,
    {data=ArrayVT(
      {data=ArrayAT(bt,{data=LDynamic({data="";pos=p3}); pos=p3}); pos=p2},ml,mt); pos=p1})
| Param(vn,vt) ->
  Param(vn,vt)

let xf_params params =
  let xf = fun ({data=param; pos=p} : param) -> {data=xf_param param; pos=p} in
  List.map xf params

let xf_fdec = function
  | FunDec(fn,rt,params,block) -> FunDec(fn,rt,xf_params params,block)
  | CExtern(fn,rt,params) -> CExtern(fn,rt,xf_params params)
  | fd -> fd


let xf_module = function
  | Module(env,fdecs) ->
    Module(env, List.map
                (fun {data=fdec; pos=p} ->
                 {data=xf_fdec fdec; pos=p}) fdecs)
