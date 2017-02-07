open Err
open Tast

type context = Context of Cast.expr

let ctx_expr (Context(e)) = e

let new_temp_var =
  let cntr = ref 0 in
  let new_temp_var'() =
    cntr := !cntr + 1;
    "m" ^ (string_of_int !cntr)
  in
  new_temp_var'

let rec transform = function
  | Tast.TCModule fdecs ->
    let f = List.map transform_fdec fdecs in
    Cast.CModule f

and transform_type = function
  | Ast.Int n when n <= Z.(~$8) -> Cast.Int8
  | Ast.Int n when n <= Z.(~$16) -> Cast.Int16
  | Ast.Int n when n <= Z.(~$32) -> Cast.Int32
  | Ast.UInt n when n <= Z.(~$8) -> Cast.UInt8
  | Ast.UInt n when n <= Z.(~$16) -> Cast.UInt16
  | Ast.UInt n when n <= Z.(~$32) -> Cast.UInt32
  | Ast.Bool -> Cast.BoolMask
  | Ast.Array { ty=t; size=s } ->
    Cast.Array { a_ty=(transform_type t); size=(Z.to_int s) }
  | _ as ty -> raise @@ TransformError ("Encountered bad type " ^ (Ast.show_ctype ty))

and transform_label = function
  | Ast.Public -> Cast.Public
  | Ast.Secret -> Cast.Secret
  | Ast.Unknown -> raise @@ TransformError "Encountered `Unknown` label"

and transform_kind = function
  | Ast.Val -> Cast.Val
  | Ast.Ref -> Cast.Ref
  | Ast.Out -> Cast.Ref

and transform_lt = function
  | { Ast.ty=t; Ast.kind=k } ->
    { Cast.ty=(transform_type t); Cast.kind=(transform_kind k)}

and transform_param { Pos.data={Ast.name=n; Ast.lt=t} } =
    {Cast.name=n; Cast.lt=transform_lt(t)}

and transform_stm' rty venv ctx stm =
  let make_expr e ty = { Cast.e=e; Cast.e_ty=ty } in
  let rset = make_expr (Cast.VarExp "rset") Cast.BoolMask in
  let get_var n =
    let lt = Env.get_var venv n in
      make_expr (Cast.VarExp n)
        (transform_type lt.Ast.ty) in
  let get_arr a i =
    let lt = Env.get_arr venv a in
      make_expr (Cast.ArrExp(a,i))
        (transform_type lt.Ast.ty) in
  let b_and l r = make_expr (Cast.BinOp(Cast.BitAnd,l,r)) Cast.BoolMask in
  let b_or l r = make_expr (Cast.BinOp(Cast.BitOr,l,r)) Cast.BoolMask in
  let b_not e = make_expr (Cast.UnOp(Cast.BitNot,e)) Cast.BoolMask in
  match stm with
  | Tast.TVarDec(v,ty,e) ->
    let ty' = transform_lt(ty) in
    let e' = transform_expr(e) in
    [Cast.VarDec(v,ty',e')]
  | Tast.TAssign(v,e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let assign_ok = b_and c (b_not rset) in
    let newval = b_and e' assign_ok in
    let oldval = b_and (get_var v) (b_not assign_ok) in
    [Cast.Assign(v,(b_or newval oldval))]
  | Tast.TArrAssign(v,i,e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let assign_ok = b_and c (b_not rset) in
    let newval = b_and e' assign_ok in
    let i' = transform_expr i in
    let oldval = b_and (get_arr v i') (b_not assign_ok) in
    [Cast.ArrAssign(v,i',(b_or newval oldval))]
  | Tast.TIf(e,bt,bf) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let tname = new_temp_var() in
    let m = make_expr (Cast.VarExp tname) Cast.BoolMask in
    let c' = b_and m c in
    let ctx' = Context(c') in
    let bt' = List.flatten(List.map (transform_stm rty bt.venv ctx') bt.body) in
    let bf' = List.flatten(List.map (transform_stm rty bf.venv ctx') bf.body) in
    let lt = { Cast.ty=Cast.BoolMask; kind=Cast.Ref } in
    let mdec = Cast.VarDec(tname,lt,b_and e' c) in
    let mnot = Cast.Assign(tname,b_not m) in
    [mdec] @ bt' @ [mnot] @ bf'
  | Tast.TFor(n,t,l,h,b) ->
    let l' = transform_expr l in
    let h' = transform_expr h in
    let b' = List.flatten(List.map (transform_stm rty b.venv ctx) b.body) in
    [Cast.For(n,l',h',b')]
  | Tast.TReturn(e) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let rval = make_expr (Cast.VarExp "rval") rty in
    let assign_ok = b_and c (b_not rset) in
    let newval = b_and e' assign_ok in
    [Cast.Assign("rval",(b_or rval newval)); Cast.Assign("rset",(b_or rset c))]
and transform_stm rty venv ctx = Pos.unpack (transform_stm' rty venv ctx)

and transform_arg = fun { Pos.data } ->
  let transform_arg' = function
    | Tast.TValArg e -> transform_expr e
    | _ -> raise @@ TransformError "transforming args not complete"
  in
    transform_arg' data

and transform_expr = fun { Pos.data } ->
  let { Tast.e; Tast.e_ty } = data in
  let ty = transform_type e_ty in
  let base_e =
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
  in { Cast.e=base_e; Cast.e_ty=ty }

and transform_primitive =
  let transform_primitive' = function
    | Tast.TNumber n -> Cast.Number (Z.to_int n) (* XXX *)
    | Tast.TBoolean true -> Cast.Mask (Cast.TRUE)
    | Tast.TBoolean false -> Cast.Mask (Cast.FALSE)
    | Tast.TArrayLiteral s -> raise @@ TransformError "array literal not yet supported"
  in
    Pos.unpack transform_primitive'

and transform_unop =
  let transform_unop' = function
    | Ast.Neg -> Cast.Neg
    | Ast.L_Not -> Cast.BitNot
    | Ast.B_Not -> Cast.BitNot
  in
    Pos.unpack transform_unop'

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
    Pos.unpack transform_binop'

and transform_fdec { Pos.data } =
  let transform_fdec' = function
    | { Tast.t_name=name; Tast.t_params=args; Tast.t_rty; Tast.t_rlbl; Tast.t_body=body } ->
      let args' = List.map transform_param args in
      let rt = { Ast.ty=t_rty; Ast.label=t_rlbl; Ast.kind=Ast.Ref } in
      let rt' = transform_lt(rt) in
      let bm_false = { Cast.ty=Cast.BoolMask; Cast.kind=Cast.Ref } in
      let prim_false = Cast.Primitive(Cast.Mask(Cast.FALSE)) in
      let bm_prim_false = { Cast.e=prim_false; Cast.e_ty=Cast.BoolMask } in
      let ctx = Context {
                  Cast.e=Cast.Primitive(Cast.Mask Cast.TRUE);
                  Cast.e_ty=Cast.BoolMask
                } in
      let body' = List.flatten(List.map (transform_stm rt'.Cast.ty body.venv ctx) body.body) in
      let r0 = { Cast.e=Cast.Primitive(Cast.Number 0); Cast.e_ty=rt'.Cast.ty } in
      let rval = Cast.VarDec("rval",rt',r0) in
      let rset = Cast.VarDec("rset",bm_false,bm_prim_false) in
      let body'' = [rval]@[rset]@body' in
        Cast.FunctionDec(name,args',rt',body'',
                         { Cast.e=Cast.VarExp("rval");
                           Cast.e_ty=rt'.Cast.ty })
  in
    transform_fdec' data
