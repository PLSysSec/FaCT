open Util
open Pos
open Err
open Tast
open Tast_util

let ____ p = raise @@ err p

let get p = function
  | Some x -> x
  | None -> raise @@ err p

let findvar vmap x =
  match Core.List.Assoc.find vmap x ~equal:vequal with
    | Some bty -> bty
    | None -> raise @@ cerr x.pos
                         "variable not defined: '%s'"
                         x.data


class typechecker =
  object (visit)
    val mutable _vmap : (var_name * base_type) list = []

    method fact_module m =
      let Ast.Module(sdecs,fdecs) = m in
      let sdecs' = List.map visit#sdec sdecs in
      let fdecs' = List.map visit#fdec fdecs in
        Module(sdecs',fdecs')

    method sdec =
      wrap @@ fun p -> function
        | Ast.StructDef (name,fields) ->
          StructDef (name,List.map visit#field fields)

    method inline = function
      | Ast.Default -> Default
      | Ast.Always  -> Always
      | Ast.Never   -> Never

    method fntype Ast.{ export; inline } =
      { export; inline=visit#inline inline; everhi=false }

    method lbl =
      wrap @@ fun p -> function
        | Ast.Public -> Public
        | Ast.Secret -> Secret

    method basic l =
      wrap @@ fun p -> function
        | Ast.BaseBool -> Bool l
        | Ast.BaseUInt s -> UInt (s,l)
        | Ast.BaseInt s -> Int (s,l)

    method mut =
      wrap @@ fun p -> function
        | Ast.R -> R
        | Ast.W -> W
        | Ast.RW -> RW

    method vattr Ast.{ cache_aligned } =
      { cache_aligned }

    method field =
      wrap @@ fun p -> function
        | Ast.Field (x,bty) -> Field (x,visit#bty bty)

    method bty =
      wrap @@ fun p -> function
        | Ast.Bool l -> Bool (visit#lbl l)
        | Ast.UInt (s,l) -> UInt (s,visit#lbl l)
        | Ast.Int (s,l) -> Int (s,visit#lbl l)
        | Ast.Ref (bty,m) -> Ref (visit#bty bty,visit#mut m)
        | Ast.Arr (bty,lexpr,vattr) -> Arr (visit#bty bty,visit#lexpr lexpr,visit#vattr vattr)
        | Ast.Struct fields -> Struct (List.map visit#field fields)
        | Ast.UVec (s,n,l) -> UVec (s,n,visit#lbl l)
        | Ast.String -> String

    method fdec =
      wrap @@ fun p -> function
        | Ast.FunDec(fn,ft,rt,params,body) ->
          let params' = List.map visit#param params in
          let body' = visit#block body in
            FunDec(fn,visit#fntype ft,rt >>= visit#bty,params',body')
        | Ast.CExtern (fn,rt,params) ->
          let params' = List.map visit#param params in
            CExtern(fn,rt >>= visit#bty,params')

    method param =
      wrap @@ fun p -> function
        | Ast.Param(x,bty) -> Param(x,visit#bty bty)

    method block blk =
      visit#stms blk

    method stms stms_ =
      List.flatten @@ List.map visit#stm stms_

    method stm stm_ =
      let p = stm_.pos in
      let stms' = visit#stm' stm_ in
      let stms' = List.map (fun s -> (make_ast p s)) stms' in
        List.map visit#stm_post stms'
    method stm_post stm = stm

    method lexpr =
      wrap @@ fun p -> function
        | Ast.LIntLiteral n -> LIntLiteral n
        | Ast.LExpression e ->
          let e' = visit#expr e in
            e' |> ignore;
            ____ p
        | Ast.LUnspecified -> ____ p

    method expr ?lookahead_bty e_ =
      let {data=(e',bty); pos} = visit#expr' lookahead_bty e_ in
        (make_ast pos e', bty)

    method _ternop p e1 e2 e3 =
      let e1' = visit#expr e1 in
      let e2',e3' =
        match Ast_util.is_untyped_int e2 with
          | Some _ ->
            let e3' = visit#expr e3 in
            let e2' = visit#expr ~lookahead_bty:(type_of e3') e2 in
              e2',e3'
          | None ->
            let e2' = visit#expr e2 in
            let e3' = visit#expr ~lookahead_bty:(type_of e2') e3 in
              e2',e3' in
      let bty1 = type_of e1' in
      let bty2 = type_of e2' in
      let bty3 = type_of e3' in
        if not (is_bool bty1) then
          raise @@ err p;
        if not (bty2 <: bty3 || bty3 <: bty2) then
          raise @@ err p;
        let l1 = label_of bty1 in
        let new_bty = bty2 +: bty3 in
        let new_bty =
          match l1.data with
            | Public -> new_bty
            | Secret -> p @>
              begin
                match new_bty.data with
                  | Bool _ -> Bool l1
                  | UInt (n,_) -> UInt (n,l1)
                  | Int (n,_) -> Int (n,l1)
                  | UVec (n,bw,_) -> UVec (n,bw,l1)
              end
        in
          (e1',e2',e3'), new_bty

    method expr' lookahead_bty =
      wrap @@ fun p -> function
        | Ast.True -> True, make_ast p @@ Bool (make_ast p Public)
        | Ast.False -> False, make_ast p @@ Bool (make_ast p Public)
        | Ast.UntypedIntLiteral n ->
          let bty = get p lookahead_bty in
            if not @@ is_integral bty then
              raise @@ err p;
            IntLiteral n, bty
        | Ast.IntLiteral (n,bty) ->
          let bty' = visit#basic (make_ast p Public) bty in
            if not @@ is_integral bty' then
              raise @@ err p;
            IntLiteral n, bty'
        | Ast.Variable x ->
          let bty = findvar _vmap x in
            Variable x, bty
        | Ast.ArrayLen e ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
          let lexpr =
            match e_bty.data with
              | Arr (_,lexpr,_) -> lexpr
              | _ -> raise @@ err p in
            begin
              match lexpr.data with
                | LIntLiteral n -> IntLiteral n, p@>UInt (32, p@>Public)
                | LDynamic x ->
                  let x_bty = findvar _vmap x in
                    Variable x, x_bty
            end
        | Ast.Cast (bty,e) ->
          let lookahead_bty = visit#basic (p@>Public) bty in
          let e' = visit#expr ~lookahead_bty e in
          let e_bty = type_of e' in
          let e_lbl = label_of e_bty in
          let c_bty = visit#basic (label_of e_bty) bty in
          let c_lbl = label_of c_bty in
            if not (is_integral e_bty || is_bool e_bty) then
              raise @@ err p;
            if not (is_integral c_bty || is_bool c_bty) then
              raise @@ err p;
            if not (e_lbl <$ c_lbl) then
              raise @@ err p;
            Cast (c_bty,e'), c_bty
        | Ast.UnOp (op,e) ->
          let e' = visit#expr e in
            UnOp(op,e'), visit#unop op e'
        | Ast.BinOp (op,e1,e2) ->
          let e1',e2' =
            match Ast_util.is_untyped_int e1 with
              | Some _ ->
                let e2' = visit#expr e2 in
                let e1' = visit#expr ~lookahead_bty:(type_of e2') e1 in
                  e1',e2'
              | None ->
                let e1' = visit#expr e1 in
                let e2' = visit#expr ~lookahead_bty:(type_of e1') e2 in
                  e1',e2'
          in
            BinOp(op,e1',e2'), visit#binop op e1' e2'
        | Ast.TernOp (e1,e2,e3) ->
          let (e1',e2',e3'),new_bty = visit#_ternop p e1 e2 e3 in
            TernOp(e1',e2',e3'), new_bty
        | Ast.Select (e1,e2,e3) ->
          let (e1',e2',e3'),new_bty = visit#_ternop p e1 e2 e3 in
            Select(e1',e2',e3'), new_bty
        | Ast.Declassify e ->
          let e' = visit#expr e in
            Declassify e', type_of e'
        | Ast.Enref x ->
          Enref x, p@>Ref (____ p, p@>RW)
        | Ast.Deref e ->
          let e' = visit#expr e in
            Deref e', ____ p
        | Ast.ArrayGet (e,lexpr) ->
          let e' = visit#expr e in
          let lexpr' = visit#lexpr lexpr in
            ArrayGet (e', lexpr'), ____ p
        | Ast.ArrayLit es ->
          ArrayLit (List.map visit#expr es), ____ p
        | Ast.ArrayZeros lexpr ->
          ArrayZeros (visit#lexpr lexpr), ____ p
        | Ast.ArrayCopy e ->
          ArrayCopy (visit#expr e), ____ p
        | Ast.ArrayView (e,index,len) ->
          ArrayView (visit#expr e,
                     visit#lexpr index,
                     visit#lexpr len), ____ p
        | Ast.Shuffle (e,ns) ->
          Shuffle (visit#expr e, ns), ____ p
        | Ast.StructLit entries ->
          StructLit (List.map
                       (fun (field,e) ->
                          (field,visit#expr e))
                       entries), ____ p
        | Ast.StructGet (e,field) ->
          StructGet (visit#expr e,field), ____ p
        | Ast.StringLiteral _ -> ____ p

    method unop op e =
      let bty = type_of e in
        match op with
          | Ast.Neg ->
            if not @@ is_integral bty then
              raise @@ err bty.pos;
            bty
          | Ast.LogicalNot ->
            if not @@ is_bool bty then
              raise @@ err bty.pos;
            bty
          | Ast.BitwiseNot ->
            if not @@ is_integral bty then
              raise @@ err bty.pos;
            bty

    method binop op e1 e2 =
      let bty1 = type_of e1 in
      let bty2 = type_of e2 in
        match op with
          | Ast.Plus
          | Ast.Minus
          | Ast.Multiply
          | Ast.Divide
          | Ast.Modulo ->
            if not @@ (is_integral bty1 || is_vec bty1) then
              raise @@ err bty1.pos;
            if not @@ (is_integral bty2 || is_vec bty2) then
              raise @@ err bty2.pos;
            if not (bty1 <: bty2) then
              raise @@ err bty2.pos;
            bty1 +: bty2
          | Ast.Equal
          | Ast.NEqual ->
            if not (is_integral bty1 || is_bool bty1 || is_vec bty1) then
              raise @@ err bty1.pos;
            if not (is_integral bty2 || is_bool bty2 || is_vec bty2) then
              raise @@ err bty2.pos;
            if not (bty1 <: bty2 || bty2 <: bty1) then
              raise @@ err bty1.pos;
            let l1 = label_of bty1 in
            let l2 = label_of bty2 in
              bty1.pos@>Bool (l1 +$ l2)
          | Ast.GT
          | Ast.GTE
          | Ast.LT
          | Ast.LTE ->
            if not (is_integral bty1) then
              raise @@ err bty1.pos;
            if not (is_integral bty2) then
              raise @@ err bty2.pos;
            if not (bty1 <: bty2 || bty2 <: bty1) then
              raise @@ err bty1.pos;
            let l1 = label_of bty1 in
            let l2 = label_of bty2 in
              bty1.pos@>Bool (l1 +$ l2)
          | Ast.LogicalAnd
          | Ast.LogicalOr ->
            if not (is_bool bty1) then
              raise @@ err bty1.pos;
            if not (is_bool bty2) then
              raise @@ err bty2.pos;
            bty1 +: bty2
          | Ast.BitwiseAnd ->
            begin
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (m,l2) ->
                  bty1.pos@>UInt (min n m,l1 +$ l2)
                | Int (n,l1),Int (m,l2) ->
                  bty1.pos@>Int (min n m,l1 +$ l2)
                | UVec (n,bw1,l1),UVec (m,bw2,l2)
                  when n = m && bw1 = bw2 ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err bty1.pos
            end
          | Ast.BitwiseOr
          | Ast.BitwiseXor ->
            begin
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (m,l2) ->
                  bty1.pos@>UInt (max n m,l1 +$ l2)
                | Int (n,l1),Int (m,l2) ->
                  bty1.pos@>Int (max n m,l1 +$ l2)
                | UVec (n,bw1,l1),UVec (m,bw2,l2)
                  when n = m && bw1 = bw2 ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err bty1.pos
            end
          | Ast.LeftShift
          | Ast.RightShift ->
            begin
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (_,l2) ->
                  bty1.pos@>UInt (n,l1 +$ l2)
                | Int (n,l1),UInt (_,l2) ->
                  bty1.pos@>Int (n,l1 +$ l2)
                | UVec (n,bw1,l1),UInt (_,l2) ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err bty1.pos
            end
          | Ast.LeftRotate
          | Ast.RightRotate ->
            begin
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (_,l2) ->
                  bty1.pos@>UInt (n,l1 +$ l2)
                | _ -> raise @@ err bty1.pos
            end

    method stm' =
      xwrap @@ fun p -> function
        | Ast.Block blk -> [Block (visit#block blk)]
        | Ast.VoidReturn
        | Ast.VarDec (_,_,_)
        | Ast.FnCall (_,_,_,_)
        | Ast.VoidFnCall (_,_)
        | Ast.Assign (_,_)
        | Ast.If (_,_,_)
        | Ast.RangeFor (_,_,_,_,_)
        | Ast.ArrayFor (_,_,_,_) | Ast.Return _ | Ast.Assume _
          -> ____ p

  end

let transform m =
  let visit = new typechecker in
    visit#fact_module m
