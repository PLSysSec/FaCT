open Pos
open Tast

#define DEBUG 0
#if (DEBUG = 1)
  #define dprintf Printf.eprintf
#else
  #define dprintf Printf.ifprintf ignore
#endif


type arrcheck = Ok | Warn | Error


(* for X, value of varname is always nonnegative *)
type term =
  | N of int
  | X of string * Q.t * int (* varname * multiplier + offset *)
  | Noterm

let show_term = function
  | N n -> string_of_int n
  | X(x,m,p) -> Printf.sprintf "(%s*%s + %d)" Q.(to_string m) x p
  | Noterm -> "noterm"

let (+.) l n =
  match l with
    | N l -> N(n + l)
    | X(x,m,p) -> X(x,m,p + n)
    | Noterm ->
      dprintf
        "add? %s +. %d\n"
        (show_term l)
        n;
      Noterm

let (+@) l1 l2 =
  match l1,l2 with
    | N l1,N l2 -> N(l1 + l2)
    | X(x,m,p),N l
    | N l,X(x,m,p) -> X(x,m,p + l)
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && Q.(m1 = m2) ->
      X(x1,m1,p1 + p2)
    | _ ->
      dprintf
        "add? %s +@ %s\n"
        (show_term l1)
        (show_term l2);
      Noterm

let ( *@ ) l1 l2 =
  match l1,l2 with
    | N l1,N l2 -> N(l1 * l2)
    | X(x,m,p),N l
    | N l,X(x,m,p) -> X(x,Q.(m * ~$l),p * l)
    | _ ->
      dprintf
        "mul? %s +@ %s\n"
        (show_term l1)
        (show_term l2);
      Noterm

let (<@) l1 l2 =
  match l1,l2 with
    | N l1,N l2 -> Some (l1 < l2)
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && Q.(m1 = m2) ->
        Some (p1 < p2)
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && p1 = 0 && p2 = 0 ->
      Some Q.(m1 < m2)
    | _ -> None
let (<=@) l1 l2 =
  let check1 = l1 <@ l2 in
  let check2 = l1 = l2 in
    match check1,check2 with
      | Some b1, b2 ->
        Some (b1 || b2)
      | _ -> None

let (>@) l1 l2 =
  match l1,l2 with
    | N l1,N l2 -> Some (l1 > l2)
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && Q.(m1 = m2) ->
        Some (p1 > p2)
    | X(x1,m1,p1),X(x2,m2,p2)
      when x1 = x2 && p1 = 0 && p2 = 0 ->
      Some Q.(m1 > m2)
    | _ -> None
let (>=@) l1 l2 =
  let check1 = l1 >@ l2 in
  let check2 = l1 = l2 in
    match check1,check2 with
      | Some b1, b2 ->
        Some (b1 || b2)
      | _ -> None


(* [base : length] *)
type t = (term * term) option

let show_range n =
  match n with
    | Some (n,l) -> Printf.sprintf "[%s : %s]" (show_term n) (show_term l)
    | None -> "[none]"

let is_contained_in n m =
  match n,m with
    | Some (n1,l1),Some (n2,l2) ->
      let ncheck = n1 >=@ n2 in
      let lcheck = (l1 +@ n1) <=@ (l2 +@ n2) in
        (match ncheck,lcheck with
          | Some b1, Some b2 ->
            dprintf
              "contain! %s <[ %s\n"
              (show_range n)
              (show_range m);
            Some (b1 && b2)
          | _ ->
            dprintf
              "contain? %s <[ %s\n"
              (show_range n)
              (show_range m);
            None)
    | _ ->
      dprintf
        "contain? %s <[ %s\n"
        (show_range n)
        (show_range m);
      None

let add_range n m =
  match n,m with
    | Some (n1,l1),Some (n2,l2) ->
      dprintf
        "add! %s + %s\n"
        (show_range n)
        (show_range m);
      Some (n1 +@ n2, (l1 +@ l2) +. (-1))
    | _ ->
      dprintf
        "add? %s + %s\n"
        (show_range n)
        (show_range m);
      None

let mul_range n' m' =
  match n',m' with
    | Some (N n1,N l1),Some (N n2,N l2) ->
      let (lo1,hi1) = (n1,n1+l1-1) in
      let (lo2,hi2) = (n2,n2+l2-1) in
      let vals = [lo1*lo2;
                  lo1*hi2;
                  hi1*lo2;
                  hi1*hi2] in
      let lo = List.fold_left min (List.hd vals) vals in
      let hi = List.fold_left max (List.hd vals) vals in
        dprintf
          "mul! %s x %s\n"
          (show_range n')
          (show_range m');
        Some (N lo, N (hi - lo + 1))
    | Some (N 0,X(x,m,p)),Some (N n2,N 1) ->
      dprintf
        "mul! %s x %s\n"
        (show_range n')
        (show_range m');
      Some (N 0, X(x, Q.(m * ~$n2), n2 * (p - 1) + 1))
    | _ ->
      dprintf
        "mul? %s x %s\n"
        (show_range n')
        (show_range m');
      None

let div_range n m =
  match n,m with
    | Some (X(x,m,0), N 1),Some (N n2, N 1) ->
      let res = X(x,Q.(m / ~$n2),0) in
        Some (res, N 1)
    | _ -> None

let shr_range n m =
  match n,m with
    | Some (X(x,m,0), N 1),Some (N n2, N 1) ->
      let n2' = 1 lsl n2 in
      let res = X(x,Q.(m / ~$n2'),0) in
        Some (res, N 1)
    | _ -> None

let lt_range n m =
  match n,m with
    | Some (N n, N 1),Some (N m, N 1) -> Some (n < m)
    | Some (N 0, N 1),Some (_, N 1) -> Some true
    | _ ->
      dprintf
        "lt? %s < %s\n"
        (show_range n)
        (show_range m);
      None

let neg_range n =
  match n with
    | Some (N n,N l) -> Some (N (1 - n - l), N l)
    | Some (X(x, m, p), N 1) ->
      Some (X (x, Q.(~-m), -p), N 1)
    | _ ->
      dprintf
        "neg? -%s\n"
        (show_range n);
      None

let check_contained n m =
  match is_contained_in n m with
    | Some true -> Ok
    | Some false ->
      dprintf "    n : %s\n    m : %s\n"
        (show_range n)
        (show_range m);
      Error
    | None -> Warn

let mk_range n m  =
  match n,m with
    | Some (n', N 1), Some (m', N 1) ->
      Some (n', m')
    | _ -> None

let of_lexpr _ranges lexpr =
  match lexpr.data with
    | LIntLiteral n -> Some (N 0, N n)
    | LDynamic x
        when List.mem_assoc x _ranges ->
        (match List.assoc x _ranges with
          | Some (len, N 1) ->
            Some (N 0, len)
          | _ -> None)
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
          | Ast.RightShift -> shr_range e1' e2'
          | _ -> None)
    | _ -> None
