open Util
open Pos
open Err
open Ast
open Ast_util
open Astmap

class constant_folder =
  object (visit)
    inherit Astmap.ast_visitor as super

    method expr_post =
      wrap @@ fun p e_ ->
      match e_ with
        | Cast (bty,e) ->
          is_untyped_int e >>=
          begin
            fun n ->
              return
                (match bty.data with
                  | BaseBool -> raise @@ err p
                  | BaseUInt s -> IntLiteral(n, make_ast bty.pos (BaseUInt s))
                  | BaseInt s -> IntLiteral(n, make_ast bty.pos (BaseInt s)))
          end >!> e_
        | UnOp (op,e1) ->
          is_untyped_int e1 >>=
          begin
            fun n ->
              return
                (match op with
                  | Neg        -> UntypedIntLiteral (~-n)
                  | BitwiseNot -> UntypedIntLiteral (lnot n)
                  | _          -> raise @@ err p)
          end >!> e_
        | BinOp (op,e1,e2) ->
          (is_untyped_int e1 >&> is_untyped_int e2) >>=
          begin
            fun (n,m) ->
              return
                (match op with
                  | Plus       -> UntypedIntLiteral (n + m)
                  | Minus      -> UntypedIntLiteral (n - m)
                  | Multiply   -> UntypedIntLiteral (n * m)
                  | Divide     -> UntypedIntLiteral (n / m)
                  | Modulo     -> UntypedIntLiteral (n mod m)
                  | BitwiseAnd -> UntypedIntLiteral (n land m)
                  | BitwiseOr  -> UntypedIntLiteral (n lor m)
                  | BitwiseXor -> UntypedIntLiteral (n lxor m)
                  | LeftShift  -> UntypedIntLiteral (n lsl m)
                  | RightShift -> UntypedIntLiteral (n asr m)
                  | Equal      -> if n =  m then True else False
                  | NEqual     -> if n <> m then True else False
                  | GT         -> if n >  m then True else False
                  | GTE        -> if n >= m then True else False
                  | LT         -> if n <  m then True else False
                  | LTE        -> if n <= m then True else False
                  | _          -> raise @@ err p)
          end >!> e_
        | _ -> e_

  end

let transform m =
  let visit = new constant_folder in
    visit#fact_module m
