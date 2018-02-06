open Pos
open Err
open Tast

#define err(p) InternalCompilerError("from source" ^ __LOC__ << p)

(* x for 'eXtract' *)
let xwrap f pa = f pa.pos pa.data
#define xfunction xwrap @@ fun p -> function

type ps_ctx_record = {
  indent : int;
}

#define ind (prindent ps_ctx 0)
#define ind1 (prindent ps_ctx 1)
let prindent ps_ctx n =
  "\n" ^ (String.make ((ps_ctx.indent + n) * 2) ' ')

#define inc ({ ps_ctx with indent=ps_ctx.indent+1 })

let ps_bty = xfunction
  | UInt n -> Printf.sprintf "uint%d" n
  | Int  n -> Printf.sprintf  "int%d" n
  | Bool   -> Printf.sprintf  "bool"
  | Num _  -> Printf.sprintf "literal"
  | String -> Printf.sprintf "string"

let ps_label' p = function
  | Public -> "public"
  | Secret -> "secret"
  | Unknown -> raise @@ err(p)

let ps_label = xfunction
  | Fixed l -> ps_label' p l
  | Guess _ -> raise @@ err(p)

let ps_mut = xfunction
  | Const -> ""
  | Mut -> "mut "

let ps_ref = xfunction
  | Const -> ""
  | Mut -> "ref "

let ps_lexpr = xfunction
  | LIntLiteral n -> string_of_int n
  | LDynamic x -> x.data

let ps_aty = xfunction
  | ArrayAT(b,lexpr) -> Printf.sprintf "%s[%s]" (ps_bty b) (ps_lexpr lexpr)

let ps_ety = xfunction
  | BaseET(b,l) -> String.concat " " [ps_label l; ps_bty b]
  | ArrayET(a,l,m) -> String.concat " " [ps_aty a; ps_label l; ps_mut m]

let ps_vty = xfunction
  | RefVT(b,l,m) -> Printf.sprintf "%s %s%s" (ps_label l) (ps_mut m) (ps_bty b)
  | ArrayVT(a,l,m) -> Printf.sprintf "%s %s%s" (ps_label l) (ps_mut m) (ps_aty a)

let ps_unop = function
  | Ast.Neg        -> "-"
  | Ast.LogicalNot -> "!"
  | Ast.BitwiseNot -> "~"

let ps_binop = function
  | Ast.Plus       -> "+"
  | Ast.Minus      -> "-"
  | Ast.Multiply   -> "*"
  | Ast.Divide     -> "/"
  | Ast.Modulo     -> "%"
  | Ast.Equal      -> "=="
  | Ast.NEqual     -> "!="
  | Ast.GT         -> ">"
  | Ast.GTE        -> ">="
  | Ast.LT         -> "<"
  | Ast.LTE        -> "<="
  | Ast.LogicalAnd -> "&&"
  | Ast.LogicalOr  -> "||"
  | Ast.BitwiseAnd -> "&"
  | Ast.BitwiseOr  -> "|"
  | Ast.BitwiseXor -> "^"
  | Ast.LeftShift  -> "<<"
  | Ast.RightShift -> ">>"

let rec ps_arg ps_ctx = xfunction
  | ByValue e -> ps_expr ps_ctx e
  | ByArray(ae,m) ->
    Printf.sprintf
      "%s%s"
      (ps_ref m)
      (ps_aexpr ps_ctx ae)
  | ByRef x ->
    Printf.sprintf
      "ref %s"
      x.data

and ps_expr' ps_ctx = function
  | True -> "true"
  | False -> "false"
  | IntLiteral n -> string_of_int n
  | Variable x -> x.data
  | ArrayGet(x,n) ->
    Printf.sprintf
      "%s[%s]"
      x.data
      (ps_expr ps_ctx n)
  | IntCast(bty,e) ->
    Printf.sprintf
      "(%s)%s"
      (ps_bty bty)
      (ps_expr ps_ctx e)
  | UnOp(op,e) ->
    Printf.sprintf
      "%s%s"
      (ps_unop op)
      (ps_expr ps_ctx e)
  | BinOp(op,e1,e2) ->
    Printf.sprintf
      "(%s %s %s)"
      (ps_expr ps_ctx e1)
      (ps_binop op)
      (ps_expr ps_ctx e2)
  | TernOp(e1,e2,e3) ->
    Printf.sprintf
      "(%s ? %s : %s)"
      (ps_expr ps_ctx e1)
      (ps_expr ps_ctx e2)
      (ps_expr ps_ctx e3)
  | Select(e1,e2,e3) ->
    Printf.sprintf
      "(%s ?? %s :: %s)"
      (ps_expr ps_ctx e1)
      (ps_expr ps_ctx e2)
      (ps_expr ps_ctx e3)
  | FnCall(f,args) ->
    Printf.sprintf
      "%s(%s)"
      f.data
      (String.concat ", " (List.map (ps_arg ps_ctx) args))
  | Declassify e ->
    Printf.sprintf
      "declassify(%s)"
      (ps_expr ps_ctx e)
  | Inject(x,stms) ->
    Printf.sprintf
      "%s <- %s" x.data
      (ps_block inc (Env.new_env(), stms))
  | _ -> "<expr>"
and ps_expr ps_ctx {data=(e,_)} = ps_expr' ps_ctx e

and ps_aexpr' ps_ctx = function
  | ArrayLit es ->
    Printf.sprintf
      "{ %s }"
      (String.concat ", "
         (List.map (ps_expr ps_ctx) es))
  | ArrayVar x -> x.data
  | ArrayZeros lexpr ->
    Printf.sprintf
      "arrzeros(%s)"
      (ps_lexpr lexpr)
  | ArrayCopy x ->
    Printf.sprintf
      "arrcopy(%s)"
      x.data
  | ArrayView(x,n,lexpr) ->
    Printf.sprintf
      "arrview(%s, %s, %s)"
      x.data
      (ps_expr ps_ctx n)
      (ps_lexpr lexpr)
  | ArrayComp _ -> "<arrcomp>"
  | _ -> "<arrexpr>"
and ps_aexpr ps_ctx {data=(ae,_)} = ps_aexpr' ps_ctx ae

and ps_stm ps_ctx = xfunction
  | BaseDec(x,bty,e) ->
    Printf.sprintf
      "%s %s = %s;"
      (ps_vty bty)
      x.data
      (ps_expr ps_ctx e)
  | ArrayDec(x,aty,ae) ->
    Printf.sprintf
      "%s %s = %s;"
      (ps_vty aty)
      x.data
      (ps_aexpr ps_ctx ae)
  | BaseAssign(x,e) ->
    Printf.sprintf
      "%s = %s;"
      x.data
      (ps_expr ps_ctx e)
  | ArrayAssign(x,n,e) ->
    Printf.sprintf
      "%s[%s] = %s;"
      x.data
      (ps_expr ps_ctx n)
      (ps_expr ps_ctx e)
  | If(e,tblock,fblock) ->
    Printf.sprintf
      "if (%s) %s else %s"
      (ps_expr ps_ctx e)
      (ps_block inc tblock)
      (ps_block inc fblock)
  | For(i,bty,lo,hi,block) ->
    Printf.sprintf
      "for (%s %s = %s to %s) %s"
      (ps_bty bty)
      i.data
      (ps_expr ps_ctx lo)
      (ps_expr ps_ctx hi)
      (ps_block inc block)
  | VoidFnCall(f,args) ->
    Printf.sprintf
      "%s(%s);"
      f.data
      (String.concat ", " (List.map (ps_arg ps_ctx) args))
  | Return e ->
    Printf.sprintf
      "return %s;"
      (ps_expr ps_ctx e)
  | VoidReturn ->
    "return;"
  | Block stms ->
    ps_block inc stms
  | _ -> "<stm>;"

and ps_block ps_ctx (_,stms) =
  let stms' = List.map (ps_stm ps_ctx) stms in
  let stms' = List.map (fun s -> ind1 ^ s) stms' in
  "{" ^ (String.concat "" stms') ^ ind ^ "}"

let ps_rty = function
  | None -> "void"
  | Some ety -> ps_ety ety

let ps_param { data=Param(x,vty) } =
  "\n    " ^ ps_vty vty ^ " " ^ x.data

let ps_fdec = xfunction
  | FunDec(f,ft,rt,params,body) ->
    let ps_ctx = { indent=0 } in
    let paramdecs = String.concat "," @@ List.map ps_param params in
      Printf.sprintf
        "%s %s(%s) %s"
        (ps_rty rt)
        f.data
        paramdecs
        (ps_block ps_ctx body)
  | _ -> ""

let ps_module (Module(fenv,fdecs)) =
  String.concat "\n\n" @@ List.map ps_fdec fdecs

let generate_pseudo fname m =
  (ps_module m)
