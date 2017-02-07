open Tast

exception TransformError of string

type context = Context of Cast.expr

let ctx_expr (Context(e)) = e

let new_temp_var =
  let cntr = ref 0 in
  let new_temp_var'() =
    cntr := !cntr + 1;
    "m" ^ (string_of_int !cntr)
  in
  new_temp_var'

let b_and l r = Cast.BinOp(Cast.BitAnd,l,r)
let b_or l r = Cast.BinOp(Cast.BitOr,l,r)
let b_not e = Cast.UnOp(Cast.BitNot,e)

let rec transform = function
  | Tast.TCModule fdecs ->
    let f = List.map transform_fdec fdecs in
    Cast.CModule f

and transform_type = function
  | Ast.Int n when n = Z.(~$32) -> Cast.Int32
  | Ast.Int n when n = Z.(~$16) -> Cast.Int16
  | Ast.Int n when n = Z.(~$8) -> Cast.Int8
  | Ast.UInt n when n = Z.(~$32) -> Cast.UInt32
  | Ast.UInt n when n = Z.(~$16) -> Cast.UInt16
  | Ast.UInt n when n = Z.(~$8) -> Cast.UInt8
  | Ast.Bool -> Cast.BoolMask
  | Ast.Array { ty=t; size=s } ->
    Cast.Array { a_ty=(transform_type t); size=(Z.to_int s) }
  | _ -> raise (TransformError "Not a valid const type")

and transform_kind = function
  | Ast.Val -> Cast.Val
  | Ast.Ref -> Cast.Ref
  | Ast.Out -> Cast.Ref

and transform_lt = function
  | { Ast.ty=t; Ast.kind=k } ->
    { Cast.ty=(transform_type t); Cast.kind=(transform_kind k)}

and transform_param { Ast.data={Ast.name=n; Ast.lt=t} } =
    {Cast.name=n; Cast.lt=transform_lt(t)}

and transform_stm' ctx = function
  | Tast.TVarDec(n,ty,v) ->
    let ty' = transform_lt(ty) in
    let v' = transform_expr(v) in
    [Cast.VarDec(n,ty',v')]
  | Tast.TAssign(n,v) ->
    let c = ctx_expr ctx in
    let v' = transform_expr(v) in
    let assign_ok = b_and c (b_not (Cast.VarExp "rset")) in
    let newval = b_and v' assign_ok in
    let oldval = b_and (Cast.VarExp n) (b_not assign_ok) in
    [Cast.Assign(n,(b_or newval oldval))]
  | Tast.TArrAssign(n,i,v) ->
    let c = ctx_expr ctx in
    let v' = transform_expr(v) in
    let assign_ok = b_and c (b_not (Cast.VarExp "rset")) in
    let newval = b_and v' assign_ok in
    let i' = transform_expr i in
    let oldval = b_and (Cast.ArrExp(n,i')) (b_not assign_ok) in
    [Cast.ArrAssign(n,i',(b_or newval oldval))]
  | Tast.TIf(e,bt,bf) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let tname = new_temp_var() in
    let m = Cast.VarExp tname in
    let c' = b_and m c in
    let ctx' = Context(c') in
    let bt' = List.flatten(List.map (transform_stm ctx') bt.body) in
    let bf' = List.flatten(List.map (transform_stm ctx') bf.body) in
    let lt = { Cast.ty=Cast.BoolMask; kind=Cast.Val } in
    let mdec = Cast.VarDec(tname,lt,b_and e' c) in
    let mnot = Cast.Assign(tname,b_not m) in
    [mdec] @ bt' @ [mnot] @ bf'
  | Tast.TFor(n,t,l,h,b) ->
    let l' = transform_expr l in
    let h' = transform_expr h in
    let b' = List.flatten(List.map (transform_stm ctx) b.body) in
    [Cast.For(n,l',h',b')]
  | Tast.TReturn(e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let rval = Cast.VarExp "rval" in
    let rset = Cast.VarExp "rset" in
    let assign_ok = b_and c (b_not rset) in
    let newval = b_and e' assign_ok in
    [Cast.Assign("rval",(b_or rval newval)); Cast.Assign("rset",(b_or rset c))]
and transform_stm ctx = Ast.unpack (transform_stm' ctx)

and transform_arg = fun { Ast.data } ->
  let transform_arg' = function
    | Tast.TValArg e -> transform_expr e
    | _ -> raise @@ TransformError "transforming args not complete"
  in
    transform_arg' data

and transform_expr = fun { Ast.data } ->
  let transform_expr'{ Tast.e } =
    match e with
      | Tast.TVarExp(s) -> Cast.VarExp s
      | Tast.TArrExp(s,i) -> Cast.ArrExp(s,transform_expr i)
      | Tast.TUnOp(u,e) -> Cast.UnOp(transform_unop(u),transform_expr(e))
      | Tast.TBinOp(b,e1,e2) ->
        let b' = transform_binop b in
        let e1' = transform_expr e1 in
        let e2' = transform_expr e2 in
        Cast.BinOp(b',e1',e2')
      | Tast.TPrimitive(p) -> Cast.Primitive(transform_primitive p)
      | Tast.TCallExp(n,args) ->
        let args' = List.map transform_arg args in
        Cast.CallExp(n,args')
  in transform_expr' data

and transform_primitive =
  let transform_primitive' = function
    | Tast.TNumber n -> Cast.Number (Z.to_int n) (* XXX *)
    | Tast.TBoolean true -> Cast.Mask (Cast.TRUE)
    | Tast.TBoolean false -> Cast.Mask (Cast.FALSE)
    | Tast.TArrayLiteral s -> raise @@ TransformError "array literal not yet supported"
  in
    Ast.unpack transform_primitive'

and transform_unop =
  let transform_unop' = function
    | Ast.Neg -> Cast.Neg
    | Ast.L_Not -> Cast.BitNot
    | Ast.B_Not -> Cast.BitNot
  in
    Ast.unpack transform_unop'

and transform_binop =
  let transform_binop' = function
    | Ast.Plus -> Cast.Plus
    | Ast.Minus -> Cast.Minus
    | Ast.Multiply -> Cast.Mult
    | Ast.GT -> Cast.GT
    | Ast.GTE -> Cast.GTE
    | Ast.LT -> Cast.LT
    | Ast.LTE -> Cast.LTE
    | Ast.Equal -> Cast.Eq
    | Ast.NEqual -> Cast.Neq
    | Ast.L_And -> Cast.BitAnd
    | Ast.L_Or -> Cast.BitOr
    | Ast.LeftShift -> Cast.LeftShift
    | Ast.RightShift -> Cast.RightShift
    | Ast.B_And -> Cast.BitAnd
    | Ast.B_Or -> Cast.BitOr
    | Ast.B_Xor -> Cast.BitXor
  in
    Ast.unpack transform_binop'

and transform_fdec { Ast.data } =
  let transform_fdec' = function
    | { Tast.t_name=name; Tast.t_params=args; Tast.t_rty; Tast.t_rlbl; Tast.t_body=body } ->
      let args' = List.map transform_param args in
      let rt = { Ast.ty=t_rty; Ast.label=t_rlbl; Ast.kind=Ast.Ref } in
      let { Cast.ty=t; Cast.kind=k } as rt' = transform_lt(rt) in
      let i8 = Cast.UInt32 (* get_rt_signedness rt *) in (* XXX *)
      let i8_lt = { Cast.ty=i8; Cast.kind=Cast.Val } in
    let bm_false = { Cast.ty=Cast.BoolMask; Cast.kind=Cast.Val } in
    let bm_prim_false = Cast.Primitive(Cast.Mask(Cast.FALSE)) in
    let ctx = Context(Cast.Primitive(Cast.Mask Cast.TRUE)) in
    let body' = List.flatten(List.map (transform_stm ctx) body.body) in
    let rval = Cast.VarDec("rval",i8_lt,Cast.Primitive(Cast.Number 0)) in
    let rset = Cast.VarDec("rset",bm_false,bm_prim_false) in
      let body'' = [rval]@[rset]@body' in
        Cast.FunctionDec(name,args',rt',body'',Cast.VarExp("rval"))
  in
    transform_fdec' data
