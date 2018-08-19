open Util
open Pos
open Err
open Tast

let sprintf = Printf.sprintf
let concat = String.concat

class pseudocode (m : fact_module) =
  object (visit)
    val mutable _indent : int = -1

    val _minfo : module_info =
      let Module(_,_,minfo) = m in minfo

    method _prindent n =
      "\n" ^ (String.make ((_indent + n) * 2) ' ')

    method fact_module () =
      let Module(sdecs,fdecs,_) = m in
      let sdecs' = List.map visit#sdec sdecs in
      let sdecs' = if sdecs' = [] then "" else "[structs]\n\n" in
      let fdecs' = List.map visit#fdec fdecs in
      let fdecs' = List.fold_left (fun a b -> a ^ "\n\n" ^ b) "" fdecs' in
        sdecs' ^ fdecs'

    method sdec =
      wrap @@ fun p ->
      fun (StructDef (name,fields)) ->
        let fields' = List.map visit#field fields in
          StructDef (name,fields')

    method field =
      wrap @@ fun p ->
      fun (Field (x,bty)) ->
        Field (x,bty)

    method fdec =
      xwrap @@ fun p -> function
        | FunDec(fn,ft,rt,params,body) ->
          let params' = concat "," @@ List.map visit#param params in
          let body' = visit#scoped body in
            sprintf "%s%s %s(%s) %s"
              (visit#fnattr ft)
              (visit#rty rt)
              fn.data
              params'
              body'
        | CExtern(fn,rt,params) ->
          let params' = concat "," @@ List.map visit#param params in
            sprintf "extern %s %s(%s);"
              (visit#rty rt)
              fn.data
              params'

    method fnattr { export; inline; } =
      sprintf "%s%s"
        (if export then "export " else "")
        (match inline with
          | Default -> ""
          | Always -> "inline "
          | Never -> "noinline ")

    method rty = function
      | None -> "void"
      | Some bt -> visit#bty bt

    method param =
      xwrap @@ fun p -> function
        | Param (x,bty) ->
          sprintf "\n    %s %s"
            (visit#bty bty)
            x.data

    method lbl =
      xwrap @@ fun p -> function
        | Public -> "public"
        | Secret -> "secret"

    method mut =
      xwrap @@ fun p -> function
        | R -> "R"
        | W -> "W"
        | RW -> "RW"

    method bty =
      xwrap @@ fun p -> function
        | Bool l -> sprintf "%s bool" (visit#lbl l)
        | UInt (s,l) -> sprintf "%s uint%d" (visit#lbl l) s
        | Int (s,l) -> sprintf "%s int%d" (visit#lbl l) s
        | Ref (bt,m) -> sprintf "%s*%s" (visit#bty bt) (visit#mut m)
        | Arr (bt,lexpr,vattr) -> sprintf "%s%s[%s]" (visit#vattr vattr) (visit#bty bt) (visit#lexpr lexpr)
        | _ -> "X[bty]X"

    method bty_nolbl =
      xwrap @@ fun p -> function
        | Bool l -> "bool"
        | UInt (s,l) -> sprintf "uint%d" s
        | Int (s,l) -> sprintf "int%d" s
        | Ref (bt,m) -> sprintf "%s*%s" (visit#bty bt) (visit#mut m)
        | Arr (bt,lexpr,vattr) -> sprintf "%s%s[%s]" (visit#vattr vattr) (visit#bty bt) (visit#lexpr lexpr)
        | _ -> "X[bty]X"

    method lexpr =
      xwrap @@ fun p -> function
        | LIntLiteral n -> string_of_int n
        | LDynamic x -> x.data

    method vattr = function
      | { cache_aligned=true } -> "cacheline "
      | _ -> ""

    method scoped blk =
      _indent <- _indent + 1;
      let stms = visit#block blk in
      let res = sprintf "{%s%s}"
                  stms
                  (if stms = "" then " " else visit#_prindent 0)
      in
        _indent <- _indent - 1;
        res

    method next nxt =
      match nxt.data with
        | Block blk -> visit#block blk
        | Return e ->
          let e' = visit#expr e in
            sprintf "%sreturn %s;"
              (visit#_prindent 1)
              e'
        | VoidReturn -> (visit#_prindent 1) ^ "return;"
        | End -> ""

    method block (blk,next) =
      match blk.data with
        | Scope blk ->
          let scoped = visit#scoped blk in
          let next = visit#next next in
            scoped ^ next
        | ListOfStuff stms ->
          let stms' = visit#stms stms in
          let next = visit#next next in
            stms' ^ next
        | If (cond,thens,elses) ->
          let cond' = visit#expr cond in
          let thens' = visit#scoped thens in
          let elses' = visit#scoped elses in
          let next = visit#next next in
            sprintf "%sif (%s) %s%s%s"
              (visit#_prindent 1)
              cond'
              thens'
              (if elses' = "{ }" then "" else " else " ^ elses')
              next
        | RangeFor (x,bty,e1,e2,blk) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
          let blk' = visit#scoped blk in
          let next = visit#next next in
            sprintf "%sfor (%s %s from %s to %s) %s%s"
              (visit#_prindent 1)
              (visit#bty bty)
              x.data
              e1'
              e2'
              blk'
              next
        | ArrayFor (x,bty,e,blk) ->
          let e' = visit#expr e in
          let blk' = visit#scoped blk in
          let next = visit#next next in
            sprintf "%sfor (%s %s in %s) %s%s"
              (visit#_prindent 1)
              (visit#bty bty)
              x.data
              e'
              blk'
              next

    method stms stms_ =
      concat ""
        (List.map
           (fun stm -> visit#_prindent 1 ^ visit#stm stm)
           stms_)

    method stm stm_ =
      match stm_.data with
        | VarDec (x,bty,e) ->
          let e' = visit#expr e in
            sprintf "%s %s = %s;"
              (visit#bty bty)
              x.data
              e'
        | FnCall (x,bty,fn,args) ->
          let args' = List.map visit#expr args in
            sprintf "%s %s = %s(%s);"
              (visit#bty bty)
              x.data
              fn.data
              (concat ", " args')
        | VoidFnCall (fn,args) ->
          let args' = List.map visit#expr args in
            sprintf "%s(%s);"
              fn.data
              (concat ", " args')
        | Assign (e1,e2) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
            sprintf "%s := %s;"
              e1' e2'
        | Assume e ->
          let e' = visit#expr e in
            sprintf "assume(%s);" e'

    method expr (e_,_) =
      match e_.data with
        | True -> "true"
        | False -> "false"
        | IntLiteral n -> string_of_int n
        | Variable x -> x.data
        | Cast (bty,e) ->
          let e' = visit#expr e in
            sprintf "%s(%s)"
              (visit#bty_nolbl bty)
              e'
        | UnOp (op,e) ->
          let e' = visit#expr e in
            sprintf "%s%s"
              (visit#unop op)
              e'
        | BinOp (op,e1,e2) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
            sprintf "(%s %s %s)"
              e1'
              (visit#binop op)
              e2'
        | TernOp (e1,e2,e3) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
          let e3' = visit#expr e3 in
            sprintf "(%s ? %s : %s)"
              e1' e2' e3'
        | Select (e1,e2,e3) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
          let e3' = visit#expr e3 in
            sprintf "(%s ?? %s :: %s)"
              e1' e2' e3'
        | Declassify e ->
          let e' = visit#expr e in
            sprintf "declassify(%s)" e'
        | Enref e ->
          sprintf "ref %s" (visit#expr e)
        | Deref e ->
          let e' = visit#expr e in
            sprintf "*%s" e'
        | ArrayGet (e,lexpr) ->
          let e' = visit#expr e in
          let lexpr' = visit#lexpr lexpr in
            sprintf "%s[%s]"
              e' lexpr'
        | ArrayLit es ->
          let es' = List.map visit#expr es in
            sprintf "[%s]"
              (concat "," es')
        | ArrayZeros lexpr ->
          sprintf "zeros(%s)" (visit#lexpr lexpr)
        | ArrayCopy e ->
          sprintf "clone(%s)" (visit#expr e)
        | ArrayView (e,index,len) ->
          sprintf "view(%s, %s, %s)"
            (visit#expr e)
            (visit#lexpr index)
            (visit#lexpr len)
        | Shuffle (e,ns) ->
          sprintf "%s<%s>"
            (visit#expr e)
            (concat "," @@ List.map string_of_int ns)
        | StructLit entries ->
          "X[structlit]X"
        | StructGet (e,field) ->
          sprintf "%s.%s"
            (visit#expr e)
            field.data
        | StringLiteral s -> sprintf "\"%s\"" s

    method unop = function
      | Ast.Neg -> "-"
      | Ast.LogicalNot -> "!"
      | Ast.BitwiseNot -> "~"

    method binop = function
      | Ast.Plus        -> "+"
      | Ast.Minus       -> "-"
      | Ast.Multiply    -> "*"
      | Ast.Divide      -> "/"
      | Ast.Modulo      -> "%"
      | Ast.Equal       -> "=="
      | Ast.NEqual      -> "!="
      | Ast.GT          -> ">"
      | Ast.GTE         -> ">="
      | Ast.LT          -> "<"
      | Ast.LTE         -> "<="
      | Ast.LogicalAnd  -> "&&"
      | Ast.LogicalOr   -> "||"
      | Ast.BitwiseAnd  -> "&"
      | Ast.BitwiseOr   -> "|"
      | Ast.BitwiseXor  -> "^"
      | Ast.LeftShift   -> "<<"
      | Ast.RightShift  -> ">>"
      | Ast.LeftRotate  -> "<<<"
      | Ast.RightRotate -> ">>>"

  end

let transform m =
  let visit = new pseudocode m in
    visit#fact_module ()
