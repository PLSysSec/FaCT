
exception TransformError of string

let rec transform = function
  | Ast.CModule fdecs ->
    let f = List.map transform_fdec fdecs in
    Cast.CModule f

and transform_type = function
  | Ast.Int -> Cast.Int
  | Ast.Bool -> Cast.Int
  | Ast.ByteArr -> raise (TransformError "Cannot convert ByteArr type")

and transform_arg {Ast.name=n; Ast.ty=t} =
  {Cast.name=n; Cast.ty=transform_type(t)}

and transform_stm_ctx ctx = function
  | Ast.VarDec(n,ty,v) as vardec -> transform_stm vardec
  | Ast.Assign(n,v) as a -> transform_stm a
  | Ast.If _ -> raise (TransformError "If transform not implemented")
  | Ast.For(n,l,h,b) as f -> transform_stm f
  | Ast.Return _ -> raise (TransformError "Return transform not implemented")

and transform_stm = function
  | Ast.VarDec(n,ty,v) ->
    let ty' = transform_type(ty) in
    let v' = transform_expr(v) in
    [Cast.VarDec(n,ty',v')]
  | Ast.Assign(n,v) -> [Cast.Assign(n,transform_expr(v))]
  | Ast.If _ -> raise (TransformError "If transform not implemented")
  | Ast.For(n,l,h,b) ->
    let l' = transform_primitive l in
    let h' = transform_primitive h in
    let b' = List.flatten(List.map transform_stm b) in
    [Cast.For(n,l',h',b')]
  | Ast.Return e ->
    let rset = Cast.Primitive(Cast.Number Int32.max_int) in
    let bnot = Cast.UnOp(Cast.BitNot,Cast.VarExp "rset") in
    let bitand = Cast.BinOp(Cast.BitAnd, transform_expr e, bnot) in
    let bitor = Cast.BinOp(Cast.BitOr,Cast.VarExp("rval"),bitand) in
    [Cast.Assign("rval", bitor); Cast.Assign("rset",rset)]

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
  | Ast.Number n -> Cast.Number (Int32.of_int n)
  | Ast.Boolean true -> Cast.Number Int32.max_int
  | Ast.Boolean false -> Cast.Number (Int32.of_int 0)
  | Ast.ByteArray s -> Cast.ByteArray s

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
    let body' = List.flatten(List.map transform_stm body) in
    let zero = Int32.of_int 0 in
    let rval = Cast.VarDec("rval",Cast.Int,Cast.Primitive(Cast.Number zero)) in
    let rset = Cast.VarDec("rset",Cast.Int,Cast.Primitive(Cast.Number zero)) in
    let body'' = [rval]@[rset]@body' in
    Cast.FunctionDec(name,args',rt',body'',Cast.VarExp("rval"))
