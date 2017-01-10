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

let codegen ctx m =
  let b = builder ctx in

  let extract_value = function
    | Ref v -> v
    | Val v -> v in

  let rec codegen_module = function
    | CModule f -> let _ = List.map codegen_fdec f in ()

  and codegen_fdec = function
    | FunctionDec(n,args,ty,body,ret) ->
      let arg_to_type { name=s; ty=t } =
        (match t with
         | Int -> i32_type ctx
         | ByteArr(n) -> pointer_type(array_type (i32_type ctx) n)) in
      let arg_types = List.map arg_to_type args in
      let arg_types' = Array.of_list arg_types in
      let ft = function_type (i32_type ctx) arg_types' in
      let the_function =
        match lookup_function n m with
        | None -> declare_function n ft m
        | Some f -> raise (Error ("Function already defined:\t" ^ n)) in
      ignore(Hashtbl.add function_params n args);
      let args_array = Array.of_list args in
      Array.iteri (fun i a ->
                    let { name=n; ty=t } = args_array.(i) in
                    set_value_name n a;
                    let a' = (match t with
                      | Int -> Val a
                      | ByteArr _ -> Ref a) in
                    Hashtbl.add named_values n a')
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
      const_array (i32_type ctx) arr'

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
        let cmp = (build_icmp Icmp.Ugt lhs rhs "gt" b) in
        build_sext cmp (i32_type ctx) "gtcmp" b
      | GTE ->
        let cmp = (build_icmp Icmp.Uge lhs rhs "gte" b) in
        build_sext cmp (i32_type ctx) "gtecmp" b
      | LT ->
        let cmp = (build_icmp Icmp.Ult lhs rhs "lt" b) in
        build_sext cmp (i32_type ctx) "ltcmp" b
      | LTE ->
        let cmp = (build_icmp Icmp.Ule lhs rhs "lte" b) in
        build_sext cmp (i32_type ctx) "ltecmp" b
      | BitAnd -> build_and lhs rhs "andtmp" b
      | BitOr -> build_or lhs rhs "ortmp" b
      | Mult -> build_mul lhs rhs "multtmp" b
      | Eq ->
        let cmp = build_icmp Icmp.Eq lhs rhs "eq" b in
        build_sext cmp (i32_type ctx) "eqtmp" b
      | Neq ->
        let cmp = (build_icmp Icmp.Ne lhs rhs "neq" b) in
        build_sext cmp (i32_type ctx) "neqtmp" b
      | LeftShift -> build_shl lhs rhs "lshift" b
      | RightShift -> build_lshr lhs rhs "rshift" b
    end

  and codegen_expr = function
    | VarExp n ->
      let v =
        (try (Hashtbl.find named_values n) with
          | Not_found -> (try Hashtbl.find loop_values n with
             | Not_found -> raise (Error ("Unknown variable: " ^ n)))) in
      (match v with
        | Val v' -> v'
        | Ref v' -> build_load v' n b)
    | ArrExp(n,i) ->
      let get_index e =
        (match e with
         | Primitive(Number n) -> const_int (i32_type ctx) n
         | VarExp n -> let v = (try Hashtbl.find loop_values n with
             | Not_found -> raise (Error ("Unknown variable: " ^ n))) in
           (match v with
            | Val v' -> v'
            | Ref v' ->
              raise (Error ("Loop variables cannot be passed by reference")))
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
        let codegen_expr' arg {name=_;ty=t} =
          let arg' = (codegen_expr arg) in
          (match t with
            | Int -> arg'
            | ByteArr n ->
              let a = build_alloca (array_type (i32_type ctx) n) "arg" b in
              ignore(build_store arg' a b);
              a) in
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
      let l' = codegen_expr (Primitive l) in
      let variable = build_phi [(l',preheader)] v b in
      Hashtbl.add loop_values v (Val variable);
      ignore(List.map codegen_stm s);
      let next_var =
        build_add variable (const_int (i32_type ctx) 1) "nextvar" b in
      let end_cond = build_icmp Icmp.Eq (codegen_prim h) next_var "loopcond" b in
      let loop_end_bb = insertion_block b in
      let after_bb = append_block ctx "postloop" the_function in
      ignore(build_cond_br end_cond after_bb loop_bb b);
      position_at_end after_bb b;
      add_incoming (next_var,loop_end_bb) variable;
    | Assign(n,e) ->
      let v = (try Hashtbl.find named_values n with
        | Not_found -> raise (Error ("Unknown variable in var assign: " ^ n))) in
      let v' = extract_value v in
      ignore(build_store (codegen_expr e) v' b)
    | ArrAssign(n,i,e) ->
      let v = (try Hashtbl.find named_values n with
        | Not_found -> raise (Error ("Unknown variable in array assign: " ^ n))) in
      let v' = extract_value v in
      let e' = codegen_expr e in
      let i_t = const_int (i32_type ctx) in
      let p = build_gep v' [| (i_t 0); (codegen_expr i)|] "ptr" b in
      ignore(build_store e' p b)
    | VarDec(n,t,e) ->
      (match t with
       | ByteArr len ->
         let arr_type = array_type (i32_type ctx) len in
         let vec = codegen_expr e in
         let alloca' = build_alloca arr_type n b in
         ignore(build_store vec alloca' b);
         Hashtbl.add named_values n (Ref alloca')
      | Int ->
        let init_val = codegen_expr e in
        let alloca = build_alloca (i32_type ctx) n b in
        ignore(build_store init_val alloca b);
        Hashtbl.add named_values n (Ref alloca);
      )

  in codegen_module
