
exception TransformError of string

let rec transform = function
  | Ast.CModule fdecs -> let _ = List.map transform_fdec fdecs in ()

and transform_type = function
  | Ast.Int -> Cast.Int
  | Ast.Bool -> Cast.Int
  | Ast.ByteArr -> raise (TransformError "Cannot convert ByteArr type")

and transform_arg {Ast.name=n; Ast.ty=t} =
  {Cast.name=n; Cast.ty=transform_type(t)}

and transform_stm = function
  | Ast.VarDec(n,ty,v) ->
    let ty' = transform_type(ty) in
    let v' = transform_expr(v) in
    Cast.VarDec(n,ty',v')
  | Ast.Assign(n,v) -> Cast.Assign(n,transform_expr(v))
  | Ast.If _ -> raise (TransformError "If transform not implemented")
  | Ast.For(n,l,h,b) ->
    let l' = transform_primitive l in
    let h' = transform_primitive h in
    let b' = List.map transform_stm b in
    Cast.For(n,l',h',b')
  | Ast.Return _ -> raise (TransformError "Return transform not implemented")

and transform_expr = function
  | Ast.VarExp s -> Cast.VarExp s
  | Ast.Unop(u,e) -> Cast.UnOp(transform_unop(u),transform_expr(e))
  | Ast.BinOp(b,e1,e2) ->
    let b' = transform_binop b in
    let e1' = transform_expr e1 in
    let e2' = transform_expr e2 in
    Cast.BinOp(b',e1',e2')
  | Ast.Primitive p -> Cast.Primitive(transform_primitive p)
  | Ast.CallExp(n,args) ->
    let args' = List.map transform_expr args in
    Cast.CallExp(n,args')

and transform_primitive = function
  | Ast.Number n -> Cast.Number n
  | Ast.Boolean true -> Cast.Number 1
  | Ast.Boolean false -> Cast.Number 0
  | Ast.ByteArray s -> raise (TransformError "Not implemented")

and transform_unop = function
  | Ast.B_Not -> Cast.BitNot

and transform_binop = function
  | Ast.Plus -> Cast.Plus
  | Ast.Minus -> Cast.Minus
  | Ast.GT -> Cast.GT
  | Ast.B_And -> Cast.BitAnd
  | Ast.B_Or -> Cast.BitOr

and transform_fdec = function
  | Ast.FunctionDec(name,args,rt,body) ->
    let args' = List.map transform_arg args in
    let rt' = transform_type(rt) in
    let body' = List.map transform_stm body in
    Cast.FunctionDec(name,args',rt',body',Cast.VarExp("TODO:GET RETURN VALUE"))
