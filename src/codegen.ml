open Llvm
open Err
open Cast
open Stdlib

let add_var = Env.add_var
let get_var = Env.get_var
let get_fn = Env.get_fn

let is_signed = function
  | Int _
  | BoolMask -> true
  | UInt _ -> false

let codegen ctx m =
  let b = builder ctx in

  let llvm_ty = function
    | Int n when n <= 8 -> i8_type ctx
    | Int n when n <= 16 -> i16_type ctx
    | Int n when n <= 32 -> i32_type ctx
    | UInt n when n <= 8 -> i8_type ctx
    | UInt n when n <= 16 -> i16_type ctx
    | UInt n when n <= 32 -> i32_type ctx
    | BoolMask -> i32_type ctx
    | _ as ty -> raise (UnclassifiedError("possibly promoted type "^(show_ctype ty)^" not supported"))
  in

  let lt_to_llvm_ty lt =
    let itype = llvm_ty lt.ty in
      match lt.kind with
        | Val -> itype
        | Ref -> pointer_type itype
        | Arr s -> pointer_type(array_type itype s)
        | DArr _ -> pointer_type(array_type itype 0)
  in

  let extend_to signed ty v =
    let llty = llvm_ty ty in
    let lb,rb = integer_bitwidth llty, integer_bitwidth @@ type_of v in
      if lb < rb then build_trunc v llty "trunctmp" b
      else if lb > rb then
        let build_ext = if signed then build_sext else build_zext in
          build_ext v llty "extendtmp" b
      else v
  in

  let rec codegen_module (CModule(fenv,f)) =
    let rec codegen_fdec = function
      | FunctionDec(n,args,vt,body,ret) ->
        let arg_to_type { lt } = lt_to_llvm_ty lt in
        let arg_types = List.map arg_to_type args in
        let arg_types' = Array.of_list arg_types in
        let rt = llvm_ty ret.e_ty in
        let ft = function_type rt arg_types' in
        let the_function =
          match lookup_function n m with
            | None -> declare_function n ft m
            | Some f -> raise (UnclassifiedError ("Function already defined:\t" ^ n)) in
        let bb = append_block ctx "entry" the_function in
          position_at_end bb b;
          allocate_args body.mem args;
          allocate_stack body;
          ignore(fold_left_params (store_args body.mem) args the_function);
          codegen_stms body;
          let ret' = codegen_ext body.venv body.mem vt.v_ty ret in
            ignore(build_ret ret' b);
            ignore(Llvm_analysis.assert_valid_function the_function);

        (* make a wrapper to handle external callers *)
        if n.[0] = '_' then
          codegen_wrapper n arg_types rt the_function

    and codegen_wrapper n arg_types rt the_function =
      let wrap_n = String.sub n 1 ((String.length n) - 1) in
      let wrap_arg_types = List.tl arg_types in
      let wrap_ft = function_type rt (Array.of_list wrap_arg_types) in
      let wrap_the_function =
        match lookup_function wrap_n m with
          | None -> declare_function wrap_n wrap_ft m
          | Some f ->
            raise (UnclassifiedError (
                     "Function already defined:\t" ^ wrap_n)) in
      let wrap_args = Array.to_list (params wrap_the_function) in
      let ctx_arg = codegen_prim BoolMask (Mask TRUE) in
      let wrap_args' = ctx_arg :: wrap_args in
      let wrap_bb = append_block ctx "entry" wrap_the_function in
        position_at_end wrap_bb b;
        let wrap_call = build_call
                          the_function
                          (Array.of_list wrap_args')
                          "calltmp" b in
          ignore(build_ret wrap_call b);
          ignore(Llvm_analysis.assert_valid_function wrap_the_function);

    and allocate_args mem args =
      let allocate_arg { name; lt } =
        match lt.kind with
          | Val
          | Ref ->
            let alloca = build_alloca (lt_to_llvm_ty lt) name b in
              add_var mem name alloca
          | Arr _
          | DArr _ -> ()
      in
        ignore(List.map allocate_arg args)

    and store_args mem args param =
      match args with
        | { name; lt }::args' ->
          begin
            match lt.kind with
              | Val
              | Ref ->
                let v = get_var mem name in
                  ignore(build_store param v b)
              | Arr _
              | DArr _ ->
                add_var mem name param
          end
        ; args'
        | _ -> raise (UnclassifiedError "store_args")

    and allocate_stack { venv; mem; body } =
      let rec allocate_stack' = function
        | VarDec(n,vt,_) ->
          let alloca = build_alloca (llvm_ty vt.v_ty) n b in
            add_var mem n alloca
        | ArrDec(n,vt,s,_) ->
          let alloca = build_alloca
                         (array_type (llvm_ty vt.v_ty) s)
                         n b in
            add_var mem n alloca
        | For(i,ty,l,h,block) ->
          let alloca = build_alloca (llvm_ty ty) i b in
            add_var mem i alloca;
            allocate_stack block
        | _ -> () in
      ignore(List.map allocate_stack' body)

    and codegen_prim ty = function
      | Number n ->
        const_int (llvm_ty ty) n
      | Mask m ->
        match m with
          | TRUE -> const_all_ones (llvm_ty ty)
          | FALSE -> const_null (llvm_ty ty)

    and codegen_unop op e =
      match op with
        | Neg -> build_neg e "negtmp" b
        | BitNot -> build_not e "nottmp" b

    and binop_unify_ty op =
      match op with
        | Plus
        | Minus
        | Mult
        | GT
        | GTE
        | LT
        | LTE
        | Eq
        | Neq -> Transform.unify_ty
        | BitAnd
        | BitOr
        | BitXor
        | LeftShift
        | RightShift -> Transform.unify_sz

    and codegen_binop op ty e1 e2 =
      match op with
        | Plus -> build_add e1 e2 "addtmp" b
        | Minus -> build_sub e1 e2 "subtmp" b
        | Mult -> build_mul e1 e2 "multtmp" b
        | GT when is_signed ty -> build_icmp Icmp.Sgt e1 e2 "gt" b
        | GTE when is_signed ty -> build_icmp Icmp.Sge e1 e2 "gte" b
        | LT when is_signed ty -> build_icmp Icmp.Slt e1 e2 "lt" b
        | LTE when is_signed ty -> build_icmp Icmp.Sle e1 e2 "lte" b
        | GT -> build_icmp Icmp.Ugt e1 e2 "gt" b
        | GTE -> build_icmp Icmp.Uge e1 e2 "gte" b
        | LT -> build_icmp Icmp.Ult e1 e2 "lt" b
        | LTE -> build_icmp Icmp.Ule e1 e2 "lte" b
        | Eq -> build_icmp Icmp.Eq e1 e2 "eq" b
        | Neq -> build_icmp Icmp.Ne e1 e2 "neq" b
        | BitAnd -> build_and e1 e2 "andtmp" b
        | BitOr -> build_or e1 e2 "ortmp" b
        | BitXor -> build_xor e1 e2 "xortmp" b
        | LeftShift -> build_shl e1 e2 "lshift" b
        | RightShift when is_signed ty -> build_ashr e1 e2 "arshift" b
        | RightShift -> build_lshr e1 e2 "lrshift" b

    and codegen_arg venv mem param = function
      | ValArg e -> codegen_ext venv mem param.ty e
      | RefArg(n,_) ->
        let nlt = get_var venv n in
          (match nlt.kind with
            | Val -> get_var mem n
            | Ref -> build_load (get_var mem n) n b
            | _ -> raise (UnclassifiedError("passing array as a ref")))
      | ArrArg(n,_,_) -> get_var mem n
      | DArrArg(n,_,_) -> get_var mem n

    and codegen_ext venv mem ty e =
      extend_to (is_signed e.e_ty) ty @@ codegen_expr venv mem e

    and codegen_expr venv mem { e; e_ty=ty } =
      match e with
      | VarExp n ->
        let nlt = get_var venv n in
        let v = get_var mem n in
          (match nlt.kind with
            | Val -> build_load v n b
            | Ref ->
              let var = build_load v n b in
                build_load var n b
            | _ -> raise (UnclassifiedError("cannot use this variable as an expression")))
      | ArrExp(n,i) ->
        let v = get_var mem n in
        let i' = codegen_expr venv mem i in
        let p = build_gep v [| const_int (i32_type ctx) 0; i' |] "ptr" b in
          build_load p (n ^ "_arrget") b
      | UnOp(op,e) ->
        let e' = codegen_expr venv mem e in
          codegen_unop op e'
      | BinOp(op,e1,e2) ->
        let ty' = binop_unify_ty op e1 e2 in
        let e1' = codegen_ext venv mem ty' e1 in
        let e2' = codegen_ext venv mem ty' e2 in
          codegen_binop op ty e1' e2'
      | Primitive p -> codegen_prim ty p
      | CallExp(callee, args) ->
        let { f_args } = get_fn fenv callee in
        let callee' =
          (match lookup_function callee m with
            | Some fn -> fn
            | None -> raise (UnclassifiedError ("Unknown function referenced: " ^ callee)))
        in
          if List.length f_args != List.length args then
            raise (UnclassifiedError("Arity mismatch for `" ^ callee ^ "`:" ^
                                     " expected " ^ string_of_int (List.length f_args) ^
                                     " but got " ^ string_of_int (List.length args)));
          let args' = List.map2 (codegen_arg venv mem) f_args args in
            build_call callee' (Array.of_list args') "calltmp" b

    and codegen_stms { venv; mem; body } =
      ignore(List.map (codegen_stm venv mem) body)

    and codegen_stm venv mem = function
      | VarDec(n,vt,e) ->
        let v = get_var mem n in
        let e' = codegen_ext venv mem vt.v_ty e in
        ignore(build_store e' v b);
      | ArrDec(n,vt,s,init) -> ()
      | Assign(n,e) ->
        let nlt = get_var venv n in
        let v = get_var mem n in
        let e' = codegen_ext venv mem nlt.ty e in
          (match nlt.kind with
            | Val -> ignore(build_store e' v b)
            | Ref ->
              let var = build_load v n b in
                ignore(build_store e' var b)
            | _ -> raise (UnclassifiedError("cannot use this variable for direct assignment")))
      | ArrAssign(n,i,e) ->
        let v = get_var mem n in
        let lt = get_var venv n in
        let i' = codegen_expr venv mem i in
        let e' = codegen_ext venv mem lt.ty e in
        let p = build_gep v [| const_int (i32_type ctx) 0; i' |] "ptr" b in
        ignore(build_store e' p b)
      | For(v,ty,l,h,s) ->
        let preheader = insertion_block b in
        let the_function = block_parent preheader in
        let bb_check = append_block ctx "loop_check" the_function in
        let bb_body = append_block ctx "loop_body" the_function in
        let bb_end = append_block ctx "loop_end" the_function in
        let i = get_var s.mem v in
        let l' = codegen_ext venv mem ty l in
          ignore(build_store l' i b);
          ignore(build_br bb_check b);

        position_at_end bb_check b;
        let i' = build_load i v b in
        let h' = codegen_ext venv mem ty h in
        let cmp = if is_signed ty then Icmp.Slt else Icmp.Ult in
        let cond = build_icmp cmp i' h' "loopcond" b in
          ignore(build_cond_br cond bb_body bb_end b);

        position_at_end bb_body b;
        codegen_stms s;
        let i' = build_load i v b in
        let one = (const_int (llvm_ty ty) 1) in
        let incr = build_add i' one "loopincr" b in
          ignore(build_store incr i b);
          ignore(build_br bb_check b);

        position_at_end bb_end b;
    in
      ignore(List.map codegen_fdec f);

  in codegen_module
