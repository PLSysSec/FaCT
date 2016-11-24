open Llvm
open Cast
open Stdlib

exception NotImplemented of string
exception Error of string

let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let codegen ctx m =
  let b = builder ctx in

  let rec codegen_module = function
    | CModule f -> let _ = List.map codegen_fdec f in ()

  and codegen_fdec = function
    | FunctionDec(n,args,ty,body,ret) ->
      let arg_to_type { name=s; ty=t } =
        (match t with
         | Int -> pointer_type(i32_type ctx)
         | ByteArr(n) -> pointer_type(array_type (i32_type ctx) n)) in
      let arg_types = List.map arg_to_type args in
      let arg_types' = Array.of_list arg_types in
      let ft = function_type (i32_type ctx) arg_types' in
      let the_function =
        match lookup_function n m with
        | None -> declare_function n ft m
        | Some f -> raise (Error ("Function already defined:\t" ^ n)) in
      let args_array = Array.of_list args in
      Array.iteri (fun i a ->
                    let { name=n; ty=_ } = args_array.(i) in
                    set_value_name n a;
                    Hashtbl.add named_values n a)
                  (params the_function);
      let bb = append_block ctx "entry" the_function in
      let _ = position_at_end bb b in
      let _ = List.map codegen_stm body in
      let ret' = (codegen_expr ret) in
      let _ = build_ret ret' b in
      let _ = Llvm_analysis.assert_valid_function the_function in
      the_function

  and codegen_prim = function
    | Number n -> const_int (i32_type ctx) n
    | ByteArray l ->
      let arr = Array.of_list l in
      let arr' = Array.map (const_int (i32_type ctx)) arr in
      let arr_type = array_type (i32_type ctx) (List.length l) in
      const_array arr_type arr'

  and codegen_unop op e =
    match op with
      | BitNot -> build_not (codegen_expr e) "nottmp" b

  and codegen_binop op e e' =
    let lhs = (codegen_expr e) in
    let rhs = (codegen_expr e') in
    begin
      match op with
      | Plus -> build_add lhs rhs "addtmp" b
      | Minus -> build_sub lhs rhs "subtmp" b
      | GT ->
        build_sext (build_icmp Icmp.Ugt lhs rhs "derp" b) (i32_type ctx) "gtcmp" b
      | BitAnd -> build_and lhs rhs "andtmp" b
      | BitOr -> build_or lhs rhs "ortmp" b
      | _ -> raise (Error "not implemented")
    end

  and codegen_expr = function
    | VarExp n ->
      let varval = (try Hashtbl.find named_values n with
        | Not_found -> raise (Error ("Unknown variable: " ^ n))) in
      build_load varval n b
    | ArrExp(n,i) ->
      let arr_val  = (try Hashtbl.find named_values n with
        | Not_found -> raise (Error ("Unknown variable: " ^ n))) in
      let i_t = const_int (i32_type ctx) in
      let p = build_gep arr_val [| (i_t 0); (i_t i)|] "ptr" b in
      (*build_extractvalue arr_val i "ptr" b*)
      build_load p "p" b
    | UnOp(op,e) -> codegen_unop op e
    | BinOp(op,e,e') -> codegen_binop op e e'
    | Primitive p -> codegen_prim p
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
        let codegen_expr' arg =
          let arg' = (codegen_expr arg) in
          const_inttoptr arg' (pointer_type (i8_type ctx)) in
        let args' = Array.map codegen_expr' (Array.of_list args) in
        build_call callee' args' "calltmp" b

  and codegen_stm = function
    | For _ -> raise (NotImplemented "Loops not implemented")
    | Assign(n,e) ->
      let v = (try Hashtbl.find named_values n with
        | Not_found -> raise (Error ("Unknown variable: " ^ n))) in
      ignore(build_store (codegen_expr e) v b)
    | ArrAssign(n,i,e) ->
      let v = (try Hashtbl.find named_values n with
        | Not_found -> raise (Error ("Unknown variable: " ^ n))) in
      let e' = codegen_expr e in
      let i_t = const_int (i32_type ctx) in
      let p = build_gep v [| (i_t 0); (i_t i)|] "ptr" b in

      (*let _ = build_insertvalue v e' i "ptr" b in*)
      ignore(build_store e' p b)
    | VarDec(n,t,e) ->
      (match t with
       | ByteArr len ->
         let arr_type = array_type (i32_type ctx) len in
         let vec = codegen_expr e in
         let alloca' = build_alloca arr_type n b in
         ignore(build_store vec alloca' b);
         Hashtbl.add named_values n alloca';
      | Int ->
        let init_val = codegen_expr e in
        let alloca = build_alloca (i32_type ctx) n b in
        ignore(build_store init_val alloca b);
        Hashtbl.add named_values n alloca;
      )

  in codegen_module
