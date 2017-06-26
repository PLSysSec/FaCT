(* Z3 helpers (jank) *)

open Z3

let vars = Hashtbl.create 10

let ctx = Z3.mk_context []
let solv = Solver.mk_simple_solver ctx

let string_of_solv () = Solver.to_string solv
let check () = Solver.check solv []
let get_model () =
  try
    Solver.get_model solv
  with
    Z3.Error _ -> None

type status = Solver.status =
  | UNSATISFIABLE
  | UNKNOWN
  | SATISFIABLE

let string_of_status = Solver.string_of_status
let string_of_model = Model.to_string

let string_of_expr = Expr.to_string

let add e = Solver.add solv [e]

let int = Arithmetic.Integer.mk_sort ctx
let bool = Boolean.mk_sort ctx
let bitvec n = BitVector.mk_sort ctx n

let true_ = Boolean.mk_true ctx
let false_ = Boolean.mk_false ctx
let num n = Arithmetic.Integer.mk_numeral_i ctx n
let var x = Hashtbl.find vars x

let new_var (sort,_) x =
  let var = Expr.mk_const_s ctx x sort in
    Hashtbl.add vars x var;
    var

let is_bv = BitVector.is_bv

let get_args = Expr.get_args

let to_bv a b =
  let a_isbv = is_bv a in
  let b_isbv = is_bv b in
    match a_isbv,b_isbv with
      | true,false ->
        let sz = BitVector.get_size @@ Expr.get_sort a in
        let b' = Arithmetic.Integer.mk_int2bv ctx sz b in
          Some (a,b')
      | false,true ->
        let sz = BitVector.get_size @@ Expr.get_sort b in
        let a' = Arithmetic.Integer.mk_int2bv ctx sz a in
          Some (a',b)
      | true,true ->
        Some (a,b)
      | false,false ->
        None

let intcast a (nsort,nsigned) (msort,_) =
  let n = BitVector.get_size nsort in
  let m = BitVector.get_size msort in
  let a_isbv = is_bv a in
  let a' =
    if not a_isbv then
      Arithmetic.Integer.mk_int2bv ctx n a
    else a
  in
    if m > n then
      let i = m - n in
        if nsigned then
          BitVector.mk_sign_ext ctx i a'
        else
          BitVector.mk_zero_ext ctx i a'
    else if m < n then
      let high = m - 1 in
      let low = 0 in
        BitVector.mk_extract ctx high low a'
    else a'

let (=) a b =
  match to_bv a b with
    | Some (a',b') ->
      Boolean.mk_eq ctx a' b'
    | None ->
      Boolean.mk_eq ctx a b

let neg a =
  if is_bv a then
    BitVector.mk_neg ctx a
  else
    Arithmetic.mk_unary_minus ctx a

let (+) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_add ctx a' b'
    | None ->
      Arithmetic.mk_add ctx [a;b]

let add_add_overflow_checks a b signed =
  begin
    add @@ BitVector.mk_add_no_overflow ctx a b signed;
    if signed then
      add @@ BitVector.mk_add_no_underflow ctx a b;
  end

let add_sub_overflow_checks a b signed =
  begin
    if signed then
      add @@ BitVector.mk_sub_no_overflow ctx a b;
    add @@ BitVector.mk_sub_no_underflow ctx a b signed;
  end

let add_mul_overflow_checks a b signed =
  begin
    add @@ BitVector.mk_mul_no_overflow ctx a b signed;
    if signed then
      add @@ BitVector.mk_mul_no_underflow ctx a b;
  end

let (-) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_sub ctx a' b'
    | None ->
      Arithmetic.mk_sub ctx [a;b]

let ( * ) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_mul ctx a' b'
    | None ->
      Arithmetic.mk_mul ctx [a;b]

let bnot a =
  if is_bv a then
    BitVector.mk_not ctx a
  else
    (neg a) - (num 1)

let (!) a = Boolean.mk_not ctx a
