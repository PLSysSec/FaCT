open Llvm
open Cast
open Stdlib

exception NotImplemented of string
exception Error of string

type var =
  | Ref of llvalue
  | Val of llvalue

let named_values:(string, var) Hashtbl.t = Hashtbl.create 10
let loop_values:(string, var) Hashtbl.t = Hashtbl.create 10
let function_params:(string, param list) Hashtbl.t = Hashtbl.create 10
let val_types:(string, ctype) Hashtbl.t = Hashtbl.create 10


let unify t t1 =
  match (t,t1) with
  | (Int32,Int32) -> Int32
  | (Int32,Int16) -> Int32
  | (Int16,Int32) -> Int32
  | (Int32,Int8) -> Int32
  | (Int8,Int32) -> Int32
  | (Int16,Int16) -> Int16
  | (Int16,Int8) -> Int16
  | (Int8,Int16) -> Int16
  | (Int8,Int8) -> Int8
  | (ByteArr x, ByteArr y) when x = y -> (ByteArr x)
  | _ -> raise (Error "Unification error in codegen")

let lt_to_var a = function
  | { ty=t; kind=Cast.Ref } -> Ref a
  | { ty=t; kind=Cast.Val } -> Val a

let lt_to_llvm_ty ctx = function
  | { ty=Int32; kind=Cast.Val } -> i32_type ctx
  | { ty=Int16; kind=Cast.Val } -> i16_type ctx
  | { ty=Int8; kind=Cast.Val } -> i8_type ctx
  | { ty=ByteArr n; kind=Cast.Val } ->
    raise(Error "Byte arrays must be a `ref` type")
  | { ty=Int32; kind=Cast.Ref } -> pointer_type(i32_type ctx)
  | { ty=Int16; kind=Cast.Ref } -> pointer_type(i16_type ctx)
  | { ty=Int8; kind=Cast.Ref } -> pointer_type(i8_type ctx)
  | { ty=ByteArr n; kind=Cast.Ref } -> pointer_type(array_type (i32_type ctx) n)

let codegen ctx m =
  let b = builder ctx in

  let extract_value = function
    | Ref v -> v
    | Val v -> v in

  let load_value n = function
    | Val v -> v
    | Ref v -> build_load v n b in

  let build_arg_call arg = function
    | { ty=Int32; kind=Cast.Val } -> arg
    | { ty=Int16; kind=Cast.Val } -> arg
    | { ty=Int8; kind=Cast.Val } -> arg
    | { ty=ByteArr n; kind=Cast.Val } ->
      let a = build_alloca (array_type (i32_type ctx) n) "arg" b in
      ignore(build_store arg a b);
      a
    | { ty=Int32; kind=Cast.Ref } ->
      let a = build_alloca (i32_type ctx) "arg" b in
      ignore(build_store arg a b);
      a
    | { ty=Int16; kind=Cast.Ref } ->
      let a = build_alloca (i16_type ctx) "arg" b in
      ignore(build_store arg a b);
      a
    | { ty=Int8; kind=Cast.Ref } ->
      let a = build_alloca (i8_type ctx) "arg" b in
      ignore(build_store arg a b);
      a
    | { ty=ByteArr n; kind=Cast.Ref } ->
      let pt = pointer_type(array_type (i32_type ctx) n) in
      let a = build_alloca pt "arg" b in
      ignore(build_store arg a b);
      a in

  let rec codegen_module = function
    | CModule f -> let _ = List.map codegen_fdec f in ()

  and codegen_fdec = function
    | FunctionDec(n,args,ty,body,ret) ->
      let arg_to_type { name=s; lt=lt } = lt_to_llvm_ty ctx lt in
      let arg_types = List.map arg_to_type args in
      let arg_types' = Array.of_list arg_types in
      let ft = function_type (lt_to_llvm_ty ctx ty) arg_types' in
      Hashtbl.add val_types n ty.ty;
      let the_function =
        match lookup_function n m with
        | None -> declare_function n ft m
        | Some f -> raise (Error ("Function already defined:\t" ^ n)) in
      ignore(Hashtbl.add function_params n args);
      let args_array = Array.of_list args in
      Array.iteri (fun i a ->
                    let { name=n; lt={ ty=t; kind=k } as lt } = args_array.(i) in
                    set_value_name n a;
                    let a' = lt_to_var a lt in
                    Hashtbl.add val_types n lt.ty;
                    Hashtbl.add named_values n a')
                  (params the_function);
      let bb = append_block ctx "entry" the_function in
      let _ = position_at_end bb b in
      let _ = List.map codegen_stm body in
      let ret' = (codegen_expr (Some ty.ty) ret) in
      let _ = build_ret ret' b in
      let _ = Llvm_analysis.assert_valid_function the_function in
      the_function

  and codegen_prim ty_ctx = function
    | Number n ->
      let ty = (match ty_ctx with
                | None -> i32_type ctx
                | Some Int32 -> i32_type ctx
                | Some Int16 -> i16_type ctx
                | Some Int8 -> i8_type ctx
                | Some (ByteArr n) -> i32_type ctx) in
      const_int ty n
    | ByteArray l ->
      let arr = Array.of_list l in
      let arr' = Array.map (const_int (i32_type ctx)) arr in
      const_array (i32_type ctx) arr'

  and codegen_unop op e =
    match op with
      | Neg -> build_neg (codegen_expr None e) "negtmp" b
      | BitNot -> build_not (codegen_expr None e) "nottmp" b

  and codegen_binop op e e' =
    let unify_binops lhs rhs lhs_ty rhs_ty =
      (match lhs_ty,rhs_ty with
        | Int32, Int32 -> lhs,rhs
        | Int32, Int16 ->
          let rhs' = build_sext rhs (i32_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Int32, Int8 ->
          let rhs' = build_sext rhs (i32_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Int16, Int32 ->
          let lhs' = build_sext lhs (i32_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Int8, Int32 ->
          let lhs' = build_sext lhs (i32_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Int16, Int16 -> lhs,rhs
        | Int16, Int8 ->
          let rhs' = build_sext rhs (i16_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Int8, Int16 ->
          let lhs' = build_sext lhs (i16_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Int8, Int8 -> lhs,rhs
        | _ -> raise (Error "Cannot unify types for binary operation")
      ) in
    let lhs = (codegen_expr None e) in
    let rhs = (codegen_expr None e') in
    let lhs_ty = expr_ty e in
    let rhs_ty = expr_ty e' in
    let lhs',rhs' = unify_binops lhs rhs lhs_ty rhs_ty in
    begin
      match op with
      | Plus -> build_add lhs' rhs' "addtmp" b
      | Minus -> build_sub lhs' rhs' "subtmp" b
      | GT ->
        let cmp = (build_icmp Icmp.Ugt lhs' rhs' "gt" b) in
        build_sext cmp (i32_type ctx) "gtcmp" b
      | GTE ->
        let cmp = (build_icmp Icmp.Uge lhs' rhs' "gte" b) in
        build_sext cmp (i32_type ctx) "gtecmp" b
      | LT ->
        let cmp = (build_icmp Icmp.Ult lhs' rhs' "lt" b) in
        build_sext cmp (i32_type ctx) "ltcmp" b
      | LTE ->
        let cmp = (build_icmp Icmp.Ule lhs' rhs' "lte" b) in
        build_sext cmp (i32_type ctx) "ltecmp" b
      | BitAnd -> build_and lhs' rhs' "andtmp" b
      | BitOr -> build_or lhs' rhs' "ortmp" b
      | BitXor -> build_xor lhs' rhs' "xortmp" b
      | Mult -> build_mul lhs' rhs' "multtmp" b
      | Eq ->
        let cmp = build_icmp Icmp.Eq lhs' rhs' "eq" b in
        build_sext cmp (i32_type ctx) "eqtmp" b
      | Neq ->
        let cmp = (build_icmp Icmp.Ne lhs' rhs' "neq" b) in
        build_sext cmp (i32_type ctx) "neqtmp" b
      | LeftShift -> build_shl lhs' rhs' "lshift" b
      | RightShift -> build_lshr lhs' rhs' "rshift" b
    end

  and prim_ty = function
    | ByteArray n -> ByteArr (List.length n)
    | Number n -> Int8 (* This will be unified to the proper type *)

  and expr_ty = function
    | VarExp n ->
      (try (Hashtbl.find val_types n) with
        | Not_found ->
          raise (Error ("Type not found for variable `" ^ n ^ "`")))
    | ArrExp(n,i) ->
      ignore((try (Hashtbl.find val_types n) with
        | Not_found ->
      raise (Error ("Type not found for variable `" ^ n ^ "`"))));
      Int32
    | UnOp(op,e) -> expr_ty e
    | BinOp(op,e,e') ->
      let ty1 = expr_ty e in
      let ty2 = expr_ty e' in
      unify ty1 ty2
    | Primitive p -> prim_ty p
    | CallExp(callee, args) ->
      (try (Hashtbl.find val_types callee) with
        | Not_found ->
          raise (Error ("Type not found for variable `" ^ callee ^ "`")))

  and codegen_expr ty_ctx = function
    | VarExp n ->
      let v =
        (try (Hashtbl.find named_values n) with
          | Not_found -> (try Hashtbl.find loop_values n with
             | Not_found -> raise (Error ("Unknown variable: " ^ n)))) in
      load_value n v
    | ArrExp(n,i) ->
      let get_index e =
        (match e with
         | Primitive(Number n) -> const_int (i32_type ctx) n
         | VarExp n -> let v = (try Hashtbl.find loop_values n with
             | Not_found -> (try Hashtbl.find named_values n with
                | Not_found -> raise (Error ("Unknown variable: " ^ n)))) in
           load_value n v
         | _ -> raise (Error "Can only access arrays with loop values or int")
        ) in
      let arr_val = (try Hashtbl.find named_values n with
          | Not_found -> raise (Error ("Unknown variable: " ^ n))) in
      let arr_val' = extract_value arr_val in
      let p = build_gep arr_val'
          [| (const_int (i32_type ctx) 0); (get_index i)|] "ptr" b in
      build_load p "p" b
    | UnOp(op,e) -> codegen_unop op e
    | BinOp(op,e,e') -> codegen_binop op e e'
    | Primitive p -> codegen_prim ty_ctx p
    | CallExp(callee, args) ->
      let callee' =
        (match lookup_function callee m with
        | Some callee -> callee
        | None -> let _ = (codegen_stdlib ctx m callee) in
          match lookup_function callee m with
          | Some callee -> callee
          | None -> raise (Error ("Unknown function referenced: " ^ callee)))
          in
        let params = params callee' in
        if Array.length params == List.length args then () else
          raise (Error("Arity mismatch for `" ^ callee ^ "`"));
        let codegen_expr' arg {name=_;lt=lt} =
          let arg' = (codegen_expr (Some lt.ty) arg) in
          build_arg_call arg' lt in
        let f_args = Array.of_list(Hashtbl.find function_params callee) in
        let args' =
          Core.Std.Array.map2_exn (Array.of_list args) f_args codegen_expr' in
        build_call callee' args' "calltmp" b

  and codegen_stm = function
    | For(v,l,h,s) ->
      let preheader = insertion_block b in
      let the_function = block_parent preheader in
      let loop_bb = append_block ctx "loop" the_function in
      ignore(build_br loop_bb b);
      ignore(position_at_end loop_bb b);
      let l' = codegen_expr None (Primitive l) in
      let variable = build_phi [(l',preheader)] v b in
      Hashtbl.add loop_values v (Val variable);
      ignore(List.map codegen_stm s);
      let next_var =
        build_add variable (const_int (i32_type ctx) 1) "nextvar" b in
      let end_cond = build_icmp Icmp.Eq (codegen_prim None h) next_var "loopcond" b in
      let loop_end_bb = insertion_block b in
      let after_bb = append_block ctx "postloop" the_function in
      ignore(build_cond_br end_cond after_bb loop_bb b);
      position_at_end after_bb b;
      add_incoming (next_var,loop_end_bb) variable;
    | Assign(n,e) ->
      let v = (try Hashtbl.find named_values n with
        | Not_found -> raise (Error ("Unknown variable in var assign: " ^ n))) in
      let v' = extract_value v in
      let ty = (try Hashtbl.find val_types n with
        | Not_found -> raise (Error ("Unknown type for var: " ^ n))) in
      ignore(build_store (codegen_expr (Some ty) e) v' b)
    | ArrAssign(n,i,e) ->
      let v = (try Hashtbl.find named_values n with
        | Not_found -> raise (Error ("Unknown variable in array assign: " ^ n))) in
      let v' = extract_value v in
      let e' = codegen_expr (Some Int32) e in
      let i_t = const_int (i32_type ctx) in
      let p = build_gep v' [| (i_t 0); (codegen_expr None i)|] "ptr" b in
      ignore(build_store e' p b)
    | VarDec(n,lt,e) ->
      let extend v lhs_ty rhs_ty =
        match lhs_ty,rhs_ty with
          | Int32,Int16 -> build_sext v (i32_type ctx) "unifytmp" b
          | Int32,Int8 -> build_sext v (i32_type ctx) "unifytmp" b
          | Int16,Int8 -> build_sext v (i16_type ctx) "unifytmp" b
          | _ -> v
        in
      let llvm_ty = lt_to_llvm_ty ctx lt in
      let v = codegen_expr (Some lt.ty) e in
      let v' = extend v lt.ty (expr_ty e) in
      let alloca = build_alloca llvm_ty n b in
      ignore(build_store v' alloca b);
      Hashtbl.add val_types n lt.ty;
      Hashtbl.add named_values n (Ref alloca)

  in codegen_module
