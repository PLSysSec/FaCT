open Pos
open Err
open Ast
open Astmap

let is_untyped_int {data=e} =
  match e with
    | UntypedIntLiteral n -> Some n
    | _ -> None

let bind f = function
  | Some n -> Some (f n)
  | None -> None

let (>>=) x f = bind f x

let (>!>) x y =
  match x with
    | Some z -> z
    | None -> y

let (>&>) n m =
  match n,m with
    | Some x, Some y -> Some (x, y)
    | _ -> None

class constant_folder =
  object (visit)
    inherit Astmap.ast_visitor as super

    method expr_post =
      wrap @@ fun p e ->
      match e with
        | UnOp (op,e1) ->
          is_untyped_int e1 >>=
          begin
            fun n ->
              match op with
                | Neg        -> UntypedIntLiteral (~-n)
                | BitwiseNot -> UntypedIntLiteral (lnot n)
                | _          -> raise @@ err p
          end >!> e
        | BinOp (op,e1,e2) ->
          (is_untyped_int e1 >&> is_untyped_int e2) >>=
          begin
            fun (n,m) ->
              match op with
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
                | _          -> raise @@ err p
          end >!> e
        | _ -> e

  end

let transform m =
  let visit = new constant_folder in
    visit#fact_module m
