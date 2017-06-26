open Pos
open Err
open Env
open Tast

#define err(p) InternalCompilerError("from source" ^ __LOC__ << p)

let wrap f pa = { pa with data=f pa.pos pa.data }
let xwrap f pa = f pa.pos pa.data

let rebind f pa = { pa with data=f pa }

#define mkpos make_ast p @@
(* p for 'uses Position' *)
#define pfunction wrap @@ fun p -> function
(* x for 'eXtract' *)
#define xfunction xwrap @@ fun p -> function



let z3e = Stack.create ()
let z3_push e = Stack.push e z3e
#define z3_pop (Stack.pop z3e)
#define z3_top (Stack.top z3e)

let z3_bty = xfunction
  | Int n -> (Z.bitvec n,true)
  | UInt n -> (Z.bitvec n,false)
  | Num k -> (Z.int,k < 0)
  | Bool -> (Z.bool,false)

let z3_ty = xfunction
  | BaseET(b,_) ->
    z3_bty b

let z3_unop = function
  | Ast.Neg -> Z.neg
  | Ast.BitwiseNot -> Z.bnot
  | Ast.LogicalNot -> Z.(!)

let z3_binop = function
  | Ast.Plus -> Z.(+)
  | Ast.Minus -> Z.(-)
  | Ast.Multiply -> Z.( * )
  | _ -> Z.(+)


(* Predicates *)

let is_int = xfunction
  | UInt _ -> true
  | Int _ -> true
  | Num _ -> true
  | _ -> false

let is_signed = xfunction
  | Int _ -> true
  | UInt _ -> false
  | Num k -> k < 0
  | _ -> raise @@ err(p)

let is_bool = xfunction
  | Bool -> true
  | _ -> false


(* Trivial conversions *)

let bconv = pfunction
  | Ast.UInt n -> UInt n
  | Ast.Int n -> Int n
  | Ast.Bool -> Bool

let mlconv = pfunction
  | Ast.Public -> Fixed Public
  | Ast.Secret -> Fixed Secret
  | Ast.Unknown -> raise (LabelError("Label inference not yet implemented!" << p))

let mconv = pfunction
  | Ast.Const -> Const
  | Ast.Mut -> Mut

let refvt_conv = pfunction
  | Ast.RefVT(b,l,m) ->
    RefVT(bconv b, mlconv l, mconv m)


(* Extraction *)

let type_of = xfunction
  | (_,ty) -> mkpos ty

let type_out = xfunction
  | BaseET(b,ml) -> (b,ml)

let expr_to_btype = xfunction
  | (_,BaseET(b,_)) -> b

let expr_to_ml = xfunction
  | (_,BaseET(_,ml)) -> ml

let expr_to_types = xfunction
  | (_,BaseET(b,ml)) -> b,ml

let refvt_to_etype' = xfunction
  | RefVT(b,ml,_) -> BaseET(b, ml)
let refvt_to_etype = rebind refvt_to_etype'


(* Subtyping *)

let (<:) { data=b1 } { data=b2 } =
  match b1,b2 with
    | UInt n, UInt m when n = m -> true
    | Int n, Int m when n = m -> true
    | Bool, Bool -> true
    | Num k, Int n -> true
    | Int n, Num k -> true
    | Num k, UInt n when k >= 0 -> true
    | UInt n, Num k when k >= 0 -> true
    | _ -> false

let join_bt p { data=b1 } { data=b2 } =
  let b' =
    match b1,b2 with
      | UInt n, UInt m when n = m -> b1
      | Int n, Int m when n = m -> b1
      | Bool, Bool -> b1
      | Num k, Int n -> b2
      | Int n, Num k -> b1
      | Num k, UInt n when k >= 0 -> b2
      | UInt n, Num k when k >= 0 -> b1
      | _ -> raise @@ err(p)
  in mkpos b'

let meet_bt p { data=b1 } { data=b2 } =
  let b' =
    match b1,b2 with
      | UInt n, UInt m when n = m -> b1
      | Int n, Int m when n = m -> b1
      | Bool, Bool -> b1
      | Num k, Int n -> b2
      | Int n, Num k -> b1
      | Num k, UInt n when k >= 0 -> b2
      | UInt n, Num k when k >= 0 -> b1
      | _ -> raise @@ err(p)
  in mkpos b'

let (<$) { data=ml1 } { data=ml2 } =
  match ml1,ml2 with
    | Fixed x, Fixed y when x = y -> true
    | Fixed Public, Fixed Secret -> true
    | _ -> false

let join_ml p { data=ml1 } { data=ml2 } =
  let ml' =
    match ml1,ml2 with
      | Fixed x, Fixed y when x = y -> ml1
      | Fixed Public, Fixed Secret
      | Fixed Secret, Fixed Public -> (Fixed Secret)
      | _ -> raise @@ err(p)
  in mkpos ml'

let (<:$) ty1 ty2 =
  let b1,ml1 = type_out ty1 in
  let b2,ml2 = type_out ty2 in
    (b1 <: b2) && (ml1 <$ ml2)

let join_ty p ty1 ty2 =
  let b1,ml1 = type_out ty1 in
  let b2,ml2 = type_out ty2 in
  let b' = join_bt p b1 b2 in
  let ml' = join_ml p ml1 ml2 in
    mkpos BaseET(b', ml')


(* Actual typechecking *)

let tc_unop' p op e =
  let b,ml = expr_to_types e in
    begin
      match op with
        | Ast.Neg
        | Ast.BitwiseNot ->
          if not (is_int b) then raise @@ err(p);
        | Ast.LogicalNot ->
          if not (is_bool b) then raise @@ err(p);
    end;
    (UnOp(op, e), BaseET(b, ml))

let tc_binop' p op e1 e2 =
  let b1,ml1 = expr_to_types e1 in
  let b2,ml2 = expr_to_types e2 in
  let b' =
    match op with
      | Ast.Plus ->
        if not (is_int b1) then raise @@ err(p);
        if not (is_int b2) then raise @@ err(p);
        let signed = (is_signed b1 || is_signed b2) in
        let z3expr = z3_top in
          begin
            if Z.is_bv z3expr then
              let [a;b] = Z.get_args z3expr in
                Z.(add_add_overflow_checks a b signed);
          end;
          join_bt p b1 b2
      | Ast.Minus ->
        if not (is_int b1) then raise @@ err(p);
        if not (is_int b2) then raise @@ err(p);
        let signed = (is_signed b1 || is_signed b2) in
        let z3expr = z3_top in
          begin
            if Z.is_bv z3expr then
              let [a;b] = Z.get_args z3expr in
                Z.(add_sub_overflow_checks a b signed);
          end;
          join_bt p b1 b2
      | Ast.Multiply ->
        if not (is_int b1) then raise @@ err(p);
        if not (is_int b2) then raise @@ err(p);
        let signed = (is_signed b1 || is_signed b2) in
        let z3expr = z3_top in
          begin
            if Z.is_bv z3expr then
              let [a;b] = Z.get_args z3expr in
                Z.(add_mul_overflow_checks a b signed);
          end;
          join_bt p b1 b2
      | Ast.GT
      | Ast.GTE
      | Ast.LT
      | Ast.LTE
      | Ast.BitwiseOr
      | Ast.BitwiseXor ->
        if not (is_int b1) then raise @@ err(p);
        if not (is_int b2) then raise @@ err(p);
        join_bt p b1 b2
      | Ast.BitwiseAnd ->
        if not (is_int b1) then raise @@ err(p);
        if not (is_int b2) then raise @@ err(p);
        meet_bt p b1 b2
      | Ast.LogicalAnd
      | Ast.LogicalOr ->
        if not (is_bool b1) then raise @@ err(p);
        if not (is_bool b2) then raise @@ err(p);
        join_bt p b1 b2
      | Ast.Equal
      | Ast.NEqual ->
        join_bt p b1 b2
      | Ast.LeftShift
      | Ast.RightShift ->
        if not (is_int b1) then raise @@ err(p);
        if not (is_int b2) then raise @@ err(p);
        { b1 with pos=p }
  in
  let ml' = join_ml p ml1 ml2 in
    (BinOp(op, e1, e2), BaseET(b', ml'))

let rec tc_expr venv = pfunction
  | Ast.True ->
    z3_push Z.true_;
    (True, BaseET(mkpos Bool, mkpos Fixed Public))
  | Ast.False ->
    z3_push Z.false_;
    (False, BaseET(mkpos Bool, mkpos Fixed Public))
  | Ast.IntLiteral n ->
    z3_push @@ Z.num n;
    (IntLiteral n, BaseET(mkpos Num n, mkpos Fixed Public))
  | Ast.Variable x ->
    z3_push @@ Z.var x.data;
    let xref = find_var venv x in
      (Variable x, refvt_to_etype' xref)
  | Ast.IntCast(b,e) ->
    let b' = bconv b in
      if not (is_int b') then raise @@ err(b'.pos);
    let e' = tc_expr venv e in
      if not (is_int (expr_to_btype e')) then raise @@ err(e'.pos);
    let ml = expr_to_ml e' in
      z3_push @@ Z.intcast z3_pop (z3_ty (type_of e')) (z3_bty b');
      (IntCast(b',e'), BaseET(b',ml))
  | Ast.Declassify e ->
    let e' = tc_expr venv e in
      (Declassify e', BaseET(expr_to_btype e', mkpos Fixed Public))
  | Ast.UnOp(op,e) ->
    let e' = tc_expr venv e in
      z3_push @@ z3_unop op @@ z3_pop;
      tc_unop' p op e'
  | Ast.BinOp(op,e1,e2) ->
    let e1' = tc_expr venv e1 in
    let a = z3_pop in
    let e2' = tc_expr venv e2 in
    let b = z3_pop in
      z3_push @@ (z3_binop op) a b;
      tc_binop' p op e1' e2'

let tc_stm venv = pfunction
  | Ast.BaseDec(x,vt,e) ->
    let e' = tc_expr venv e in
    let ety = type_of e' in
    let vt' = refvt_conv vt in
    let xty = refvt_to_etype vt' in
      if not (ety <:$ xty) then raise @@ err(e'.pos);
      add_var venv x vt';
      let zvar = Z.new_var (z3_ty xty) x.data in
        Z.(add (zvar = z3_pop));
      BaseDec(x,vt',e')

let tc_fdec = pfunction
  | Ast.FunDec(fn,rt,params,stms) ->
    let venv = Env.new_env () in
      FunDec(fn,rt,params,List.map (tc_stm venv) stms)

let tc_module (Ast.Module fdecs) =
  let r = (Module (List.map tc_fdec fdecs)) in
    Core.Out_channel.write_all "overflow.z3"
      ~data:((Z.string_of_solv ())^"\n");
  let status = Z.check () in
  let model = Z.get_model () in
  let model_output =
    (match model with
      | Some m ->
        (Z.string_of_model m)^"\n"
      | None -> "Unsatisfiable\n")
  in
    Core.Out_channel.write_all "overflow_model.z3"
      ~data:model_output;
    match status with
      | Z.SATISFIABLE -> r
      | _ -> raise @@ SMTSolverError "possible overflow somewhere"
