open Pos
open Tast


type arrcheck = Ok | Warn | Error


(* for X, value of varname is always nonnegative *)
type term =
  | N of int
  | X of string * Q.t * int (* varname * multiplier + offset *)
  | None

let show_term = function
  | N n -> string_of_int n
  | X(x,m,p) -> Printf.sprintf "(%s*%s + %d)" Q.(to_string m) x p

let (+.) l n =
  match l with
    | N l -> N(n + l)
    | X(x,m,p) -> X(x,m,p + n)

let (+@) l1 l2 =
  match l1,l2 with
    | N l1,N l2 -> N(l1 + l2)
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && Q.(m1 = m2) ->
      X(x1,m1,p1 + p2)
    | _ -> None

let (<@) l1 l2 =
  match l1,l2 with
    | N l1,N l2 -> l1 < l2
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && Q.(m1 = m2) ->
        p1 < p2
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && p1 = 0 && p2 = 0 ->
      Q.(m1 < m2)
    | _ -> false
let (<=@) l1 l2 = (l1 <@ l2) || (l1 = l2)

let (>@) l1 l2 =
  match l1,l2 with
    | N l1,N l2 -> l1 > l2
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && Q.(m1 = m2) ->
        p1 > p2
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && p1 = 0 && p2 = 0 ->
      Q.(m1 > m2)
    | _ -> false
let (>=@) l1 l2 = (l1 >@ l2) || (l1 = l2)


(* [base : length] *)
type t = (term * term) option

let show_range n =
  match n with
    | Some (n,l) -> Printf.sprintf "[%s : %s]" (show_term n) (show_term l)
    | None -> "[none]"

let is_contained_in n m =
  match n,m with
    | Some (n1,l1),Some (n2,l2) ->
      Some (n1 >=@ n2 && (l1 +@ n1) <=@ (l2 +@ n2))
    | _ -> None

let add_range n m =
  match n,m with
    | Some (n1,l1),Some (n2,l2) ->
      Some (n1 +@ n2, (l1 +@ l2) +. (-1))
    | _ -> None

let mul_range n m =
  match n,m with
    | Some (N n1,N l1),Some (N n2,N l2) ->
      let (lo1,hi1) = (n1,n1+l1-1) in
      let (lo2,hi2) = (n2,n2+l2-1) in
      let vals = [lo1*lo2;
                  lo1*hi2;
                  hi1*lo2;
                  hi1*hi2] in
      let lo = List.fold_left min (List.hd vals) vals in
      let hi = List.fold_left max (List.hd vals) vals in
      Some (N lo, N (hi - lo + 1))
    | _ -> None

let neg_range n =
  match n with
    | Some (N n,N l) -> Some (N (-n), N (1 - n - l))
    | _ -> None

let check_contained n m =
  match is_contained_in n m with
    | Some true -> Ok
    | Some false ->
      Printf.eprintf "    n : %s\n    m : %s\n"
        (show_range n)
        (show_range m);
      Error
    | None -> Warn

let of_lexpr lexpr =
  match lexpr.data with
    | LIntLiteral n -> Some (N 0, N n)
    (*| LDynamic x -> Some (N 0, X(x.data,Q.one,0))*)
    | _ -> None

let rec of_expr _ranges (e : expr) =
  match e.data with
    | IntLiteral n,_ -> Some (N n, N 1)
    | Lvalue {data=(Base n,_)},_
      when List.mem_assoc n _ranges ->
      List.assoc n _ranges
    | BinOp(op,e1,e2),_ ->
      let e1' = of_expr _ranges e1 in
      let e2' = of_expr _ranges e2 in
        (match op with
          | Ast.Plus -> add_range e1' e2'
          | Ast.Minus -> add_range e1' (neg_range e2')
          | Ast.Multiply -> mul_range e1' e2'
          | _ -> None)
    | _ -> None
