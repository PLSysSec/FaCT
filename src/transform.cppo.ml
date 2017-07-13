open Pos
open Err
open Tast

#define err(p) InternalCompilerError("from source" ^ __LOC__ << p)

let wrap f pa = { pa with data=f pa.pos pa.data }
let xwrap f pa = f pa.pos pa.data

let rebind f pa = { pa with data=f pa }

#define mkpos make_ast p @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function



let new_temp_var =
  let ctr = ref 0 in
  let new_temp_var' () =
    ctr := !ctr + 1;
    "__m" ^ (string_of_int !ctr)
  in
  new_temp_var'

let is_secret e =
  let { data=(_,BaseET(_,{data=Fixed l})) } = e in
    l = Secret

let rec xf_arg' venv { data; pos=p } =
  match data with
    | ByValue e ->
      let e' = xf_expr venv e in
        ByValue e'
    | ByRef _ -> data
and xf_arg venv pa = { pa with data=xf_arg' venv pa }

and xf_expr' venv { data; pos=p } =
  let (e, ety) = data in
    match e with
      | True
      | False
      | IntLiteral _
      | Variable _
      | ArrayLen _ -> e
      | ArrayGet(x,e) ->
        let e' = xf_expr venv e in
          ArrayGet(x,e')
      | IntCast(bt,e) ->
        let e' = xf_expr venv e in
          IntCast(bt,e')
      | UnOp(op,e) ->
        let e' = xf_expr venv e in
          UnOp(op,e')
      | BinOp(op,e1,e2) ->
        let e1' = xf_expr venv e1 in
        let e2' = xf_expr venv e2 in
          begin
            match op with
              | Ast.LogicalAnd ->
                if is_secret e1' then
                  Select(e1',e2',mkpos (False, BaseET(mkpos Bool, mkpos Fixed Public)))
                else
                  BinOp(op,e1',e2')
              | Ast.LogicalOr ->
                if is_secret e1' then
                  Select(e1',mkpos (True, BaseET(mkpos Bool, mkpos Fixed Public)),e2')
                else
                  BinOp(op,e1',e2')
              | _ -> BinOp(op,e1',e2')
          end
      | TernOp(e1,e2,e3) ->
        let e1' = xf_expr venv e1 in
        let e2' = xf_expr venv e2 in
        let e3' = xf_expr venv e3 in
        if is_secret e1 then
          Select(e1',e2',e3')
        else
          TernOp(e1',e2',e3')
      | Select _ -> raise @@ err(p)
      | FnCall(f,args) ->
        let args' = List.map (xf_arg venv) args in
          FnCall(f,args')
      | Declassify e ->
        let e' = xf_expr venv e in
          Declassify e'
and xf_expr venv ({ data=(e,ety) } as pa) = { pa with data=(xf_expr' venv pa, ety) }

let rec xf_stm' venv p = function
  | BaseDec(x,vt,e) ->
    let e' = xf_expr venv e in
      [BaseDec(x,vt,e')]
  | BaseAssign(x,e) ->
    let e' = xf_expr venv e in
      [BaseAssign(x,e')]
  | If(cond,thenstms,elsestms) ->
    let cond' = xf_expr venv cond in
      if is_secret cond' then
        let vt = mkpos RefVT(mkpos Bool, mkpos Fixed Secret, mkpos Const) in
        let tname = mkpos new_temp_var () in
        let mdec = () (* BaseDec a secret bool *) in
          Env.add_var venv tname (* blah *);
        let mnot = mkpos () (* BaseAssign not tname && ~ctx~ *) in
        (* XXX *)
        let thenstms' = xf_block thenstms in
        let elsestms' = xf_block elsestms in
        [If(cond',thenstms',elsestms')]
      else
        let thenstms' = xf_block thenstms in
        let elsestms' = xf_block elsestms in
        [If(cond',thenstms',elsestms')]
  | For(i,ity,lo,hi,stms) ->
    let lo' = xf_expr venv lo in
    let hi' = xf_expr venv hi in
    let stms' = xf_block stms in
      [For(i,ity,lo',hi',stms')]
  | VoidFnCall(f,args) ->
    let args' = List.map (xf_arg venv) args in
      [VoidFnCall(f,args')]
  | Return e ->
    let e' = xf_expr venv e in
      (* XXX *)
      [Return e']
  | VoidReturn -> [VoidReturn]
and xf_stm venv pa = List.map (make_ast pa.pos) (xf_stm' venv pa.pos pa.data)

and xf_block (venv,stms) =
  let stms' = List.flatten @@ List.map (xf_stm venv) stms in
    (venv, stms')

let xf_fdec fenv = pfunction
  | FunDec(f,rt,params,stms) ->
    FunDec(f,rt,params,xf_block stms)

let xf_module (Module(fenv,fdecs)) =
  let fenv = Env.new_env () in
  let ret = Module(fenv, List.map (xf_fdec fenv) fdecs) in
    ret


