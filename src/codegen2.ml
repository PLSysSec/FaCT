open Llvm
open Cast
open Stdlib

exception NotImplemented of string
exception Error of string

let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let codegen ctx m =
  let b = builder ctx in

  (* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
let create_entry_block_alloca the_function var_name =
  let b' = builder_at ctx (instr_begin (entry_block the_function)) in
  build_alloca (double_type ctx) var_name b' in

  let rec codegen_module = function
    | CModule f -> let _ = List.map codegen_fdec f in ()

  and codegen_fdec = function
    | FunctionDec(n,args,ty,body,ret) ->
      let doubles = Array.make (List.length args) (i32_type ctx) in
      let ft = function_type (i32_type ctx) doubles in
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
      let _ = build_ret (codegen_expr ret) b in
      let _ = Llvm_analysis.assert_valid_function the_function in
      the_function

  and codegen_prim = function
    | Number n -> const_int (i32_type ctx) (Int32.to_int n)
    | ByteArray str -> build_global_stringptr str "" b

  and codegen_unop op e =
    match op with
    | BitNot -> build_neg (codegen_expr e) "nottmp" b

  and codegen_binop op e e' =
    let lhs = (codegen_expr e) in
    let rhs = (codegen_expr e') in
    begin
      match op with
      | Plus -> build_add lhs rhs "addtmp" b
      | Minus -> build_sub lhs rhs "subtmp" b
      | GT -> build_icmp Icmp.Ugt lhs rhs "cmptmp" b
      | BitAnd -> build_and lhs rhs "andtmp" b
      | BitOr -> build_or lhs rhs "ortmp" b
      | _ -> raise (Error "not implemented")
    end

  and codegen_expr = function
    | VarExp v ->
      (try Hashtbl.find named_values v with
       | Not_found -> raise (Error ("Unknown variable:\t" ^ v)))
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
          | None -> raise (Error ("Unknown function referenced:\t" ^ callee)))
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
    | Assign(n,e) -> raise (NotImplemented "Mutation not implemented yet")
    | VarDec(n,_,e) ->
      let e' = codegen_expr e in
      set_value_name n e';
      Hashtbl.add named_values n e';
      e'

  in codegen_module
