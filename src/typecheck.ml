open Util
open Pos
open Err
open Tast
open Astmap

let ____ p = raise @@ err p

let get p = function
  | Some x -> x
  | None -> raise @@ err p

let type_of (e_, bty) = bty

let vequal x y = x.data = y.data

let findvar vmap x =
  match Core.List.Assoc.find vmap x ~equal:vequal with
    | Some bty -> bty
    | None -> raise @@ cerr(Printf.sprintf
                              "variable not defined: '%s'"
                              x.data, x.pos)

let is_integral =
  xwrap @@ fun p -> function
    | UInt _ -> true
    | Int _ -> true
    | _ -> false

let is_bool =
  xwrap @@ fun p -> function
    | Bool _ -> true
    | _ -> false

let rec label_of =
  xwrap @@ fun p -> function
    | Bool l
    | UInt (_,l)
    | Int (_,l)
    | UVec (_,_,l) -> l
    | Ref (bty,_)
    | Arr (bty,_,_) -> label_of bty
    | Struct _ -> ____ p
    | String -> p@>Public

let (<$) l1 l2 =
  match l1.data,l2.data with
    | Secret,Public -> false
    | _ -> true



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

    method stm' =
      xwrap @@ fun p -> function
        | Ast.Block blk -> [Block (visit#block blk)]

    method stm stm_ =
      let p = stm_.pos in
      let stms' = visit#stm' stm_ in
      let stms' = List.map (fun s -> (make_ast p s)) stms' in
        List.map visit#stm_post stms'
    method stm_post stm = stm

    method expr ?lookahead_bty e_ =
      let {data=(e',bty); pos} = visit#expr' lookahead_bty e_ in
        (make_ast pos e', bty)

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
          let e' = visit#expr e in
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
            e' |> ignore;
            ____ p
        | Ast.BinOp (op,e1,e2) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
            (e1', e2') |> ignore;
            ____ p
        | Ast.TernOp (e1,e2,e3) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
          let e3' = visit#expr e3 in
            (e1', e2', e3') |> ignore;
            ____ p
        | Ast.Select (e1,e2,e3) ->
          let e1' = visit#expr e1 in
          let e2' = visit#expr e2 in
          let e3' = visit#expr e3 in
            (e1', e2', e3') |> ignore;
            ____ p
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

    method lexpr =
      wrap @@ fun p -> function
        | Ast.LIntLiteral n -> LIntLiteral n
        | Ast.LExpression e ->
          let e' = visit#expr e in
            e' |> ignore;
            ____ p
        | Ast.LUnspecified -> ____ p
  end

let transform m =
  let visit = new typechecker in
    visit#fact_module m
