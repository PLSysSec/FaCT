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

let rec unify t t1 =
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
  | (UInt32,UInt32) -> UInt32
  | (UInt32,UInt16) -> UInt32
  | (UInt16,UInt32) -> UInt32
  | (UInt32,UInt8) -> UInt32
  | (UInt8,UInt32) -> UInt32
  | (UInt16,UInt16) -> UInt16
  | (UInt16,UInt8) -> UInt16
  | (UInt8,UInt16) -> UInt16
  | (UInt8,UInt8) -> UInt8
  | (Array a, Array b) when a.size = b.size ->
    Array { size=a.size; a_ty=(unify a.a_ty b.a_ty)}
  | _ -> raise (Error "Unification error in codegen")

let lt_to_var a = function
  | { ty=t; kind=Cast.Ref } -> Ref a
  | { ty=t; kind=Cast.Val } -> Val a

let rec lt_to_llvm_ty ctx = function
  | { ty=Int32; kind=Cast.Val } -> i32_type ctx
  | { ty=Int16; kind=Cast.Val } -> i16_type ctx
  | { ty=Int8; kind=Cast.Val } -> i8_type ctx
  | { ty=UInt32; kind=Cast.Val } -> i32_type ctx
  | { ty=UInt16; kind=Cast.Val } -> i16_type ctx
  | { ty=UInt8; kind=Cast.Val } -> i8_type ctx
  | { ty=Array _; kind=Cast.Val } ->
    raise(Error "Byte arrays must be a `ref` type")
  | { ty=Int32; kind=Cast.Ref } -> pointer_type(i32_type ctx)
  | { ty=Int16; kind=Cast.Ref } -> pointer_type(i16_type ctx)
  | { ty=Int8; kind=Cast.Ref } -> pointer_type(i8_type ctx)
  | { ty=UInt32; kind=Cast.Ref } -> pointer_type(i32_type ctx)
  | { ty=UInt16; kind=Cast.Ref } -> pointer_type(i16_type ctx)
  | { ty=UInt8; kind=Cast.Ref } -> pointer_type(i8_type ctx)
  | { ty=Array a; kind=Cast.Ref } ->
    let lt' = { ty=a.a_ty; kind=Cast.Val } in
    let pt = pointer_type(array_type (lt_to_llvm_ty ctx lt') a.size) in
    pt

let extend v lhs_ty rhs_ty ctx b =
  match lhs_ty,rhs_ty with
    | Some Int32, Some Int16 -> 
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some Int32, Some Int8 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some Int16, Some Int8 ->
      build_sext v (i16_type ctx) "unifytmp" b, (i16_type ctx)
    | Some UInt32, Some UInt16 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some UInt32, Some UInt8 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some UInt16, Some UInt8 ->
      build_sext v (i16_type ctx) "unifytmp" b, (i16_type ctx)
    | Some Int16, Some Int32 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some Int8, Some Int32 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some Int8, Some Int16 ->
      build_sext v (i16_type ctx) "unifytmp" b, (i16_type ctx)
    | Some UInt16, Some UInt32 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some UInt8, Some UInt32 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some UInt8, Some UInt16 ->
      build_sext v (i16_type ctx) "unifytmp" b, (i16_type ctx)
    | Some Int32, Some Int32 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some Int16, Some Int16 ->
      build_sext v (i16_type ctx) "unifytmp" b, (i16_type ctx)
    | Some Int8, Some Int8 ->
      build_sext v (i8_type ctx) "unifytmp" b, (i8_type ctx)
    | Some UInt32, Some UInt32 ->
      build_sext v (i32_type ctx) "unifytmp" b, (i32_type ctx)
    | Some UInt16, Some UInt16 ->
      build_sext v (i16_type ctx) "unifytmp" b, (i16_type ctx)
    | Some UInt8, Some UInt8 ->
      build_sext v (i8_type ctx) "unifytmp" b, (i8_type ctx)
    | _ -> raise (Error "Cannot extend an unsigned and signed int")

let extend_to v ty ctx b = extend v ty ty ctx b

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
    | { ty=UInt32; kind=Cast.Val } -> arg
    | { ty=UInt16; kind=Cast.Val } -> arg
    | { ty=UInt8; kind=Cast.Val } -> arg
    | { ty=Array a; kind=Cast.Val } as lt ->
      let lt' = { lt with ty=a.a_ty } in
      let a =
        build_alloca (array_type (lt_to_llvm_ty ctx lt') a.size) "arg" b in
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
    | { ty=UInt32; kind=Cast.Ref } ->
      let a = build_alloca (i32_type ctx) "arg" b in
      ignore(build_store arg a b);
      a
    | { ty=UInt16; kind=Cast.Ref } ->
      let a = build_alloca (i16_type ctx) "arg" b in
      ignore(build_store arg a b);
      a
    | { ty=UInt8; kind=Cast.Ref } ->
      let a = build_alloca (i8_type ctx) "arg" b in
      ignore(build_store arg a b);
      a
    | { ty=Array a; kind=Cast.Ref } as lt ->
      let lt' = { lt with ty=a.a_ty } in
      let pt = pointer_type(array_type (lt_to_llvm_ty ctx lt') a.size) in
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
                    let { name=n; lt={ ty=t; kind=k } as lt } =
                      args_array.(i) in
                    set_value_name n a;
                    let a' = lt_to_var a lt in
                    Hashtbl.add val_types n lt.ty;
                    Hashtbl.add named_values n a')
                  (params the_function);
      let bb = append_block ctx "entry" the_function in
      let _ = position_at_end bb b in
      let _ = List.map codegen_stm body in
      let ret' = (codegen_expr (Some ty.ty) ret) in
      let ret'',_ = extend_to ret' (Some ty.ty) ctx b in
      let _ = build_ret ret'' b in
      let _ = Llvm_analysis.assert_valid_function the_function in
      the_function

  and codegen_prim ty_ctx = function
    | Number n ->
      let is_array = function
        | Array _ -> true
        | _ -> false in
      let ty = (match ty_ctx with
                | None -> i32_type ctx
                | Some Int32 -> i32_type ctx
                | Some Int16 -> i16_type ctx
                | Some Int8 -> i8_type ctx
                | Some UInt32 -> i32_type ctx
                | Some UInt16 -> i16_type ctx
                | Some UInt8 -> i8_type ctx
                | Some (Array a) ->
                  let k = (if is_array a.a_ty then Cast.Ref else Cast.Val) in
                  let lt = { kind=k; ty=a.a_ty } in 
                  lt_to_llvm_ty ctx lt) in
      const_int ty n
    | ByteArray l ->
      let arr = Array.of_list l in
      let arr' = Array.map (const_int (i32_type ctx)) arr in
      const_array (i32_type ctx) arr'

  and codegen_unop op ty_ctx e =
    match op with
      | Neg -> build_neg (codegen_expr ty_ctx e) "negtmp" b
      | BitNot -> build_not (codegen_expr ty_ctx e) "nottmp" b

  and codegen_binop ty_ctx op e e' =
    let truncate_or_extend_rhs rhs lhs_ty rhs_ty =
      match lhs_ty,rhs_ty with
        | Some Int32, Some Int32 -> rhs
        | Some Int32, Some Int16 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
        | Some Int32, Some Int8 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
        | Some Int16, Some Int32 ->
          build_trunc rhs (i16_type ctx) "rhstrunctmp" b
        | Some Int16, Some Int16 -> rhs
        | Some Int16, Some Int8 -> build_sext rhs (i16_type ctx) "rhssexttmp" b
        | Some Int8, Some Int32 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
        | Some Int8, Some Int16 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
        | Some Int8, Some Int8 -> rhs
        | None, Some Int32 ->
          begin
            match ty_ctx with
              | Some Int32 -> rhs
              | Some Int16 -> build_trunc rhs (i16_type ctx) "rhstrunctmp" b
              | Some Int8 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | None, Some Int16 ->
          begin
            match ty_ctx with
              | Some Int32 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
              | Some Int16 -> rhs
              | Some Int8 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | None, Some Int8 ->
          begin
            match ty_ctx with
              | Some Int32 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
              | Some Int16 -> build_sext rhs (i16_type ctx) "rhssexttmp" b
              | Some Int8 -> rhs
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | Some Int32, None ->
          begin
            match ty_ctx with
              | Some Int32 -> rhs
              | Some Int16 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
              | Some Int8 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | Some Int16, None ->
          begin
            match ty_ctx with
              | Some Int32 -> build_trunc rhs (i16_type ctx) "rhstrunctmp" b
              | Some Int16 -> rhs
              | Some Int8 -> build_sext rhs (i16_type ctx) "rhssexttmp" b
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | Some Int8, None ->
          begin
            match ty_ctx with
              | Some Int32 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
              | Some Int16 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
              | Some Int8 -> rhs
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | Some UInt32, Some UInt32 -> rhs
        | Some UInt32, Some UInt16 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
        | Some UInt32, Some UInt8 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
        | Some UInt16, Some UInt32 ->
          build_trunc rhs (i16_type ctx) "rhstrunctmp" b
        | Some UInt16, Some UInt16 -> rhs
        | Some UInt16, Some UInt8 -> build_sext rhs (i16_type ctx) "rhssexttmp" b
        | Some UInt8, Some UInt32 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
        | Some UInt8, Some UInt16 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
        | Some UInt8, Some UInt8 -> rhs
        | None, Some UInt32 ->
          begin
            match ty_ctx with
              | Some UInt32 -> rhs
              | Some UInt16 -> build_trunc rhs (i16_type ctx) "rhstrunctmp" b
              | Some UInt8 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | None, Some UInt16 ->
          begin
            match ty_ctx with
              | Some UInt32 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
              | Some UInt16 -> rhs
              | Some UInt8 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | None, Some UInt8 ->
          begin
            match ty_ctx with
              | Some UInt32 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
              | Some UInt16 -> build_sext rhs (i16_type ctx) "rhssexttmp" b
              | Some UInt8 -> rhs
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | Some UInt32, None ->
          begin
            match ty_ctx with
              | Some UInt32 -> rhs
              | Some UInt16 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
              | Some UInt8 -> build_sext rhs (i32_type ctx) "rhssexttmp" b
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | Some UInt16, None ->
          begin
            match ty_ctx with
              | Some UInt32 -> build_trunc rhs (i16_type ctx) "rhstrunctmp" b
              | Some UInt16 -> rhs
              | Some UInt8 -> build_sext rhs (i16_type ctx) "rhssexttmp" b
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | Some UInt8, None ->
          begin
            match ty_ctx with
              | Some UInt32 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
              | Some UInt16 -> build_trunc rhs (i8_type ctx) "rhstrunctmp" b
              | Some UInt8 -> rhs
              | _ -> raise (Error "Cannot truncate or extend for shift")
          end
        | _ -> rhs
    in
    let unify_binops lhs rhs lhs_ty rhs_ty =
      (match lhs_ty,rhs_ty with
        | Some Int32, Some Int32 -> lhs,rhs
        | Some Int32, Some Int16 ->
          let rhs' = build_sext rhs (i32_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some Int32, Some Int8 ->
          let rhs' = build_sext rhs (i32_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some Int16, Some Int32 ->
          let lhs' = build_sext lhs (i32_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Some Int8, Some Int32 ->
          let lhs' = build_sext lhs (i32_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Some Int16, Some Int16 -> lhs,rhs
        | Some Int16, Some Int8 ->
          let rhs' = build_sext rhs (i16_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some Int8, Some Int16 ->
          let lhs' = build_sext lhs (i16_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Some Int8, Some Int8 -> lhs,rhs
        | Some Int32, None ->
          let rhs' = build_sext rhs (i32_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some Int16, None ->
          let rhs' = build_sext rhs (i16_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some Int8, None ->
          let rhs' = build_sext rhs (i8_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | None, Some Int32 ->
          let lhs' = build_sext lhs (i32_type ctx) "lhssexttmp" b in
          lhs',rhs
        | None, Some Int16 ->
          let lhs' = build_sext lhs (i16_type ctx) "lhssexttmp" b in
          lhs',rhs
        | None, Some Int8 ->
          let lhs' = build_sext lhs (i8_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Some UInt32, Some UInt32 -> lhs,rhs
        | Some UInt32, Some UInt16 ->
          let rhs' = build_sext rhs (i32_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some UInt32, Some UInt8 ->
          let rhs' = build_sext rhs (i32_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some UInt16, Some UInt32 ->
          let lhs' = build_sext lhs (i32_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Some UInt8, Some UInt32 ->
          let lhs' = build_sext lhs (i32_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Some UInt16, Some UInt16 -> lhs,rhs
        | Some UInt16, Some UInt8 ->
          let rhs' = build_sext rhs (i16_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some UInt8, Some UInt16 ->
          let lhs' = build_sext lhs (i16_type ctx) "lhssexttmp" b in
          lhs',rhs
        | Some UInt8, Some UInt8 -> lhs,rhs
        | Some UInt32, None ->
          let rhs' = build_sext rhs (i32_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some UInt16, None ->
          let rhs' = build_sext rhs (i16_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | Some UInt8, None ->
          let rhs' = build_sext rhs (i8_type ctx) "rhssexttmp" b in
          lhs,rhs'
        | None, Some UInt32 ->
          let lhs' = build_sext lhs (i32_type ctx) "lhssexttmp" b in
          lhs',rhs
        | None, Some UInt16 ->
          let lhs' = build_sext lhs (i16_type ctx) "lhssexttmp" b in
          lhs',rhs
        | None, Some UInt8 ->
          let lhs' = build_sext lhs (i8_type ctx) "lhssexttmp" b in
          lhs',rhs
        | None, None ->
          let ty =
            (match ty_ctx with
              | Some Int32 -> i32_type ctx
              | Some Int16 -> i16_type ctx
              | Some Int8 -> i8_type ctx
              | Some UInt32 -> i32_type ctx
              | Some UInt16 -> i16_type ctx
              | Some UInt8 -> i8_type ctx
              | _ -> raise (Error "Unknown LHS type for binary unification"))
          in
          let lhs' = build_sext lhs ty "lhssexttmp" b in
          let rhs' = build_sext rhs ty "rhssexttmp" b in
          lhs',rhs'
        | _ -> raise (Error "Cannot unify types for binary operation")
      ) in
    let lhs_ty = expr_ty ty_ctx e in
    let rhs_ty = expr_ty ty_ctx e' in
    let ty = match ty_ctx with
              | Some ty_ctx -> ty_ctx
              | None -> raise (Error "Cannot determine type for binop") in
    let lhs = (codegen_expr (Some ty) e) in
    let rhs = (codegen_expr (Some ty) e') in
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
      | LeftShift -> 
        let rhs' = truncate_or_extend_rhs rhs lhs_ty rhs_ty in
        build_shl lhs rhs' "lshift" b
      | RightShift ->
        let rhs' = truncate_or_extend_rhs rhs lhs_ty rhs_ty in
        build_lshr lhs rhs' "rshift" b
    end

  and prim_ty ty_ctx = function
    | ByteArray n ->
      (match ty_ctx with
        | None -> raise (Error "Cannot determine type stored in array")
        | Some ty -> Some(Array { size=(List.length n); a_ty=ty }))
    | Number n -> ty_ctx

  and expr_ty ty_ctx = function
    | VarExp n ->
      Some (try (Hashtbl.find val_types n) with
        | Not_found ->
          raise (Error ("Type not found for variable `" ^ n ^ "`")))
    | ArrExp(n,i) ->
      let ty = (try (Hashtbl.find val_types n) with
        | Not_found ->
      raise (Error ("Type not found for variable `" ^ n ^ "`"))) in
      (match ty with
        | Array a -> Some a.a_ty
        | _ -> raise (Error "Expected an array"))
    | UnOp(op,e) -> expr_ty ty_ctx e
    | BinOp(op,e,e') ->
      let ty1 = expr_ty ty_ctx e in
      let ty2 = expr_ty ty_ctx e' in
      begin
        match ty1,ty2 with
          | Some ty1', Some ty2' -> Some(unify ty1' ty2')
          | Some ty1', None -> Some ty1'
          | None, Some ty2' -> Some ty2'
          | None, None -> ty_ctx
      end
    | Primitive p -> prim_ty ty_ctx p
    | CallExp(callee, args) ->
      Some (try (Hashtbl.find val_types callee) with
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
          [| (const_int (i32_type ctx) 0); (get_index i) |] "ptr" b in
      let b = build_load p "p" b in
      b
    | UnOp(op,e) -> codegen_unop op ty_ctx e
    | BinOp(op,e,e') -> codegen_binop ty_ctx op e e'
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
      let i32 = Some Int32 in
      let l' = codegen_expr i32 l in
      let h' = codegen_expr i32 h in
      let variable = build_phi [(l',preheader)] v b in
      Hashtbl.add loop_values v (Val variable);
      Hashtbl.add val_types v Int32;
      ignore(List.map codegen_stm s);
      let next_var =
        build_add variable (const_int (i32_type ctx) 1) "nextvar" b in
      let end_cond =
        build_icmp Icmp.Eq h' next_var "loopcond" b in
      let loop_end_bb = insertion_block b in
      let after_bb = append_block ctx "postloop" the_function in
      ignore(build_cond_br end_cond after_bb loop_bb b);
      position_at_end after_bb b;
      add_incoming (next_var,loop_end_bb) variable;
    | Assign(n,e) ->
      let ty = (try Hashtbl.find val_types n with
        | Not_found -> raise (Error ("Unknown type for var: " ^ n))) in
      let expr = codegen_expr (Some ty) e in
      let ty' = expr_ty (Some ty) e in
      let ty'' = match ty' with 
        | Some t -> t
        | None -> raise (Error "Unknown type") in
      let expr',_ = extend expr (Some ty) ty' ctx b in
      let llvm_ty'' = lt_to_llvm_ty ctx { ty=ty''; kind=Val } in
      let alloc = build_alloca llvm_ty'' n b in
      ignore(build_store expr' alloc b);
      Hashtbl.replace named_values n (Ref alloc)
    | ArrAssign(n,i,e) ->
      let v = (try Hashtbl.find named_values n with
        | Not_found ->
          raise (Error ("Unknown variable in array assign: " ^ n))) in
      let ty_ctx = match Hashtbl.find val_types n with
                     | Array { a_ty=ty } -> ty
                     | _ -> raise (Error "Expected an array") in
      let v' = extract_value v in
      let e' = codegen_expr (Some ty_ctx) e in
      let expr',_ = extend_to e' (Some ty_ctx) ctx b in
      let i_t = const_int (i32_type ctx) in
      let p = build_gep v' [| (i_t 0); (codegen_expr (Some Int32) i)|] "ptr" b in
      ignore(build_store expr' p b);
    | VarDec(n,lt,e) ->
      let v = codegen_expr (Some lt.ty) e in
      let v', ty' = extend v (Some lt.ty) (expr_ty (Some lt.ty) e) ctx b in
      let alloca = build_alloca ty' n b in
      ignore(build_store v' alloca b);
      Hashtbl.add val_types n lt.ty;
      Hashtbl.add named_values n (Ref alloca)

  in codegen_module
