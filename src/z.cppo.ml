(* Z3 helpers (jank) *)

open Z3

#define err (Err.NotImplemented(__LOC__))

let vars = Hashtbl.create 10

let ctx = Z3.mk_context []
let solv = Solver.mk_simple_solver ctx

let string_of_solv () = Solver.to_string solv
let push () = Solver.push solv
let pop () = Solver.pop solv 1
let popn n = Solver.pop solv n
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

(* for when you just need a thing *)
let thing (sort,_) =
  if Z3.Sort.equal sort bool then
    true_
  else
    Z3.Expr.mk_numeral_int ctx 4 sort

let bv0 n = Expr.mk_numeral_int ctx 0 (bitvec n)

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

let (!) a = Boolean.mk_not ctx a

let (=) a b =
  match to_bv a b with
    | Some (a',b') ->
      Boolean.mk_eq ctx a' b'
    | None ->
      Boolean.mk_eq ctx a b

let (!=) a b = !(a = b)

let (>) a b = Arithmetic.mk_gt ctx a b
let (>=) a b = Arithmetic.mk_ge ctx a b
let (<) a b = Arithmetic.mk_lt ctx a b
let (<=) a b = Arithmetic.mk_le ctx a b

let (||) a b = Boolean.mk_or ctx [a;b]
let (&&) a b = Boolean.mk_and ctx [a;b]
let (=>) a b = Boolean.mk_implies ctx a b

let ugt a b = BitVector.mk_ugt ctx a b
let uge a b = BitVector.mk_uge ctx a b
let ult a b = BitVector.mk_ult ctx a b
let ule a b = BitVector.mk_ule ctx a b
let sgt a b = BitVector.mk_sgt ctx a b
let sge a b = BitVector.mk_sge ctx a b
let slt a b = BitVector.mk_slt ctx a b
let sle a b = BitVector.mk_sle ctx a b

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

let add_neg_overflow_check a =
  add @@ BitVector.mk_neg_no_overflow ctx a

let add_add_overflow_check a b signed =
  begin
    (* XXX incorrect and incomplete *)
    (*let sz = BitVector.get_size @@ Expr.get_sort a in
    let zero = bv0 sz in
      add ((sge a zero) && (sge b zero));
      add (slt (a + b) zero);
      let bad_val = new_var (bitvec sz,signed) "bad" in
      let bad = (bad_val = a + b) in
        add bad;
        (match check () with
          | SATISFIABLE ->
            let Some m = get_model () in
              print_endline (string_of_solv ());
              print_endline (string_of_model m);
              raise @@ err
          | _ -> ());
        pop ();*)
        add @@ BitVector.mk_add_no_overflow ctx a b signed;
        if signed then
          add @@ BitVector.mk_add_no_underflow ctx a b;
  end

let add_sub_overflow_check a b signed =
  begin
    if signed then
      add @@ BitVector.mk_sub_no_overflow ctx a b;
    add @@ BitVector.mk_sub_no_underflow ctx a b signed;
  end

let add_mul_overflow_check a b signed =
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

let (!.) a =
  if is_bv a then
    BitVector.mk_not ctx a
  else
    (neg a) - (num 1)

let (|.) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_or ctx a' b'
    | None ->
      raise err
let (&.) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_and ctx a' b'
    | None ->
      raise err
let (^.) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_xor ctx a' b'
    | None ->
      raise err

let (>>) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_ashr ctx a' b'
    | None ->
      raise err
let (>>.) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_lshr ctx a' b'
    | None ->
      raise err
let (<<) a b =
  match to_bv a b with
    | Some (a',b') ->
      BitVector.mk_shl ctx a' b'
    | None ->
      raise err

let ite a b c = Boolean.mk_ite ctx a b c
