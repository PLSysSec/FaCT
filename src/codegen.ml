open Tast
open Llvm
open Pos
open Env

exception CodegenError
exception FunctionAlreadyDefined

let counter = ref 0

let fake_pos = { file=""; line=0; lpos=0; rpos=0 }

(* Start env functionality. This is here because of circular dependencies. TODO: move it out *)
type fentry = { ret_ty:Tast.ret_type; args:Tast.params }
[@@deriving show]

type fenv = (Tast.fun_name,fentry) Hashtbl.t [@printer pp_hashtbl]
[@@deriving show]

let new_fenv () = Hashtbl.create 10

let has_fn = Hashtbl.mem

let get_fn fenv f =
  try
    Hashtbl.find fenv f
  with
    Not_found -> raise @@ Err.errFnNotDefined f

let add_fn = Hashtbl.add

(* End env functionality *)

let is_signed = function
  | UInt _ -> false
  | Int _  -> true
  | Bool   -> raise CodegenError
  | Num _  -> raise CodegenError

let get_size = function
  | LIntLiteral s -> s
  | LUnspecified  -> raise CodegenError

let bt_to_llvm_ty ctx = function
  | UInt size when size <= 8  -> i8_type ctx
  | UInt size when size <= 16 -> i16_type ctx
  | UInt size when size <= 32 -> i32_type ctx
  | Int  size when size <= 8  -> i8_type ctx
  | Int  size when size <= 16 -> i16_type ctx
  | Int  size when size <= 32 -> i32_type ctx
  | Bool                      -> i32_type ctx (* TODO: Double check this*)
  | Num(i,b)                  -> i32_type ctx (* TODO: Double check semantics for `Num` *)

let vt_to_llvm_ty ctx = function
  | RefVT({data=base_type},maybe_label,_) ->
    bt_to_llvm_ty ctx base_type
  | ArrayVT({data=ArrayAT(at,size)},maybe_label,_) ->
    let arr_ty = bt_to_llvm_ty ctx at.data in
    let size = get_size size.data in
    pointer_type(array_type arr_ty size)

let param_to_type ctx = function
  | { data=Param(var_name,var_type) } -> vt_to_llvm_ty ctx var_type.data

let expr_ty_to_llvm_ty ctx = function
  | BaseET({data=base_type},_) ->
    bt_to_llvm_ty ctx base_type
  | ArrayET({data=ArrayAT(at,size)},_,_) ->
    let arr_ty = bt_to_llvm_ty ctx at.data in
    let size = get_size size.data in
    array_type arr_ty size (* TODO: Double check that this is not a pointer*)

let expr_ty_to_base_ty = function
  | BaseET(bt,_) -> bt
  | ArrayET _ -> raise CodegenError

let get_ret_ty ctx = function
  | None -> raise CodegenError
  | Some ty -> expr_ty_to_llvm_ty ctx ty.data

(* Allocate all of the args for a function *)
let allocate_args ctx llbuilder var_env args =
  let allocate_arg ({data=Param(var_name,var_type)} as arg) =
    let llvm_ty = param_to_type ctx arg in
    let alloca = build_alloca llvm_ty var_name.data llbuilder in
    add_var var_env var_name alloca
  in
  ignore(List.map allocate_arg args)

(* Allocate space for each variable declared inside a function. This is
   done at the beginning of the function. *)
let rec allocate_stack ctx builder venv stms =
  let allocate_stack' = function
    | {data=BaseDec(var_name,var_type,expr)} ->
       let llvm_ty = vt_to_llvm_ty ctx var_type.data in
       let alloca = build_alloca llvm_ty var_name.data builder in
       add_var venv var_name alloca
    | {data=ArrayDec(var_name,var_type,expr)} ->
      let llvm_ty = vt_to_llvm_ty ctx var_type.data in
      (* TODO: I think this will fail. I think this will just allocate a
         pointer. But we want it to allocate the space for the array. 
         So yea, fix dis *)
      let alloca = build_alloca llvm_ty var_name.data builder in
      add_var venv var_name alloca
    | {data=If(cond,thenstms,elsestms)} ->
      allocate_stack ctx builder venv thenstms;
      allocate_stack ctx builder venv elsestms
    | {data=For(var_name,base_type,low,high,stms)} ->
      let llvm_ty = bt_to_llvm_ty ctx base_type.data in
      let alloca = build_alloca llvm_ty var_name.data builder in
      (* TODO: Fix the scoping here. This will force us to use a different
         var name for the loop iterator for each for loop per function. We
         probably want to be able to reuse this?? *)
      add_var venv var_name alloca;
      allocate_stack ctx builder venv stms
    | _ -> ()
  in
  let env,stms' = stms in
  ignore(List.map allocate_stack' stms')

let codegen_binop op e1 e2 ty b =
  match op with
    | Ast.Plus -> build_add e1 e2 "addtmp" b
    | Ast.Minus -> build_sub e1 e2 "subtmp" b
    | Ast.Multiply -> build_mul e1 e2 "multmp" b
    | Ast.Equal -> build_icmp Icmp.Eq e1 e2 "eqtmp" b
    | Ast.NEqual -> build_icmp Icmp.Ne e1 e2 "neqtmp" b
    | Ast.GT -> build_icmp Icmp.Ugt e1 e2 "gttmp" b
    | Ast.GTE -> build_icmp Icmp.Uge e1 e2 "gtetmp" b
    | Ast.LT -> build_icmp Icmp.Ult e1 e2 "lttmp" b
    | Ast.LTE -> build_icmp Icmp.Ule e1 e2 "ltetmp" b
    | Ast.LogicalAnd -> raise CodegenError
    | Ast.LogicalOr -> raise CodegenError
    | Ast.BitwiseAnd -> build_and e1 e2 "andtmp" b
    | Ast.BitwiseOr -> build_or e1 e2 "ortmp" b
    | Ast.BitwiseXor -> build_xor e1 e2 "xortmp" b
    | Ast.LeftShift -> build_shl e1 e2 "lshift" b
    | Ast.RightShift when is_signed ty -> build_ashr e1 e2 "arshift" b 
    | Ast.RightShift -> build_lshr e1 e2 "lrshift" b

let codegen_unop builder value = function
  | Ast.Neg -> build_neg value "negtmp" builder
  | Ast.LogicalNot -> build_not value "lnottmp" builder
  | Ast.BitwiseNot -> build_not value "bnottmp" builder

let rec expr_to_ty = function
  | True -> raise CodegenError
  | False -> raise CodegenError
  | IntLiteral(i) -> raise CodegenError
  | Variable(var_name) -> raise CodegenError
  | ArrayGet(var_name,expr) -> raise CodegenError
  | ArrayLen(var_name) -> raise CodegenError
  | IntCast(base_ty,expr) -> raise CodegenError
  | UnOp(op,expr) -> raise CodegenError
  | BinOp(op,expr1,expr2) -> raise CodegenError
  | TernOp(expr1,expr2,expr3) -> raise CodegenError
  | FnCall(fun_name, arg_exprs) -> raise CodegenError
  | Declassify(expr) -> raise CodegenError

let build_cast ctx value = (* from, to *) function
  | Bool -> const_intcast value (bt_to_llvm_ty ctx Bool) ~is_signed:false
  | UInt(n) -> const_intcast value (bt_to_llvm_ty ctx (UInt n)) ~is_signed:false
  | Int(n) -> const_intcast value (bt_to_llvm_ty ctx (Int n)) ~is_signed:true
  | _ -> raise CodegenError (* TODO: How do we cast the Num type?? *)

(*
let arg_ty_to_expr_ty = function
  | RefVT(bt, lab, mut) -> BaseET(bt, lab)
  | ArrayVT(at, lab, mut) -> ArrayET(at, lab, mut)
*)

let param_ty_to_llvm_ty = function
  | _ -> raise CodegenError

let size_of_lexpr = function
  | LIntLiteral n -> n
  | LUnspecified  -> raise CodegenError

let rec codegen_arg llcontext llmodule builder venv fenv tenv arg ty =
  match arg.data with
    | ByValue expr -> codegen_expr llcontext llmodule builder venv fenv tenv expr.data 
    | ByArray arr ->
      begin
        match arr.data with
          | ArrayZeros lexpr ->
            begin
              match lexpr.data with
                | LIntLiteral n ->
                  let llty = param_ty_to_llvm_ty ty in
                  let zero = const_int llty 0 in
                  let zeros = Array.make n zero in
                  let arr_ty = array_type llty n in
                  let arr = const_array arr_ty zeros in
                  build_array_alloca arr_ty arr "zerodarray" builder
                | LUnspecified -> raise CodegenError
            end
          | ArrayCopy var_name ->
            let arr = find_var venv var_name in
            let arr_ty = type_of arr in
            let arr_load = build_load arr "arrcopyload" builder in
            build_array_alloca arr_ty arr_load "arrcopy" builder (* Probs gonna segfault *)
          | ArrayView(var_name, expr, lexpr) ->
            (* TODO: I think the transformations should do the heavy lifting here and the
                     TAST should get rid of ArrayView *)
            let arr = find_var venv var_name in
            let arr_load = build_load arr "arrviewload" builder in
            let _ = match lookup_function "memset" llmodule with
              | Some fn -> fn
              | None ->
                let i32_ty = i32_type llcontext in
                let ptr_ty = pointer_type (i8_type llcontext) in
                let bool_ty = i1_type llcontext in
                let arg_types = [| ptr_ty; ptr_ty; i32_ty; i32_ty; bool_ty |] in
                let memcpy = "llvm.memcpy.p0i8.i32" in
                declare_function memcpy (function_type (void_type llcontext) arg_types) llmodule in
              let dest_name = make_ast fake_pos ("dest" ^ (string_of_int !counter)) in
              ignore(counter := !counter + 1);
              let arr_ty = find_var tenv var_name in
              let size = make_ast fake_pos (LIntLiteral(array_length (type_of arr))) in
              let zeros = make_ast fake_pos (ArrayZeros(size)) in
              let dest_ast = make_ast fake_pos (ArrayDec(dest_name, arr_ty, zeros)) in
              ignore(codegen_stm llcontext llmodule builder None venv fenv tenv dest_ast);
              let fun_name = make_ast fake_pos "memset" in
              let dest = make_ast fake_pos (ByRef dest_name) in
              let src = make_ast fake_pos (ByRef var_name) in
              let i32 = make_ast fake_pos (UInt(32)) in
              let unknown_label = make_ast fake_pos (Fixed Unknown) in
              let int_type = BaseET(i32, unknown_label) in
              let lsize = make_ast fake_pos (IntLiteral(size_of_lexpr lexpr.data), int_type) in
              let lsize' = make_ast fake_pos (ByValue lsize) in
              let alignment = make_ast fake_pos (IntLiteral 0, int_type) in
              let alignment' = make_ast fake_pos (ByValue alignment) in
              let i1 = make_ast fake_pos (UInt(1)) in
              let bool_type = BaseET(i1, unknown_label) in
              let volatile = make_ast fake_pos (IntLiteral 0, bool_type) in
              let volatile' = make_ast fake_pos (ByValue volatile) in
              let at,l,m = match arr_ty.data with
                | ArrayVT(at,l,m) -> at,l,m
                | RefVT _ -> raise CodegenError in
              let arr_et = ArrayET(at,l,m) in
              let fn_call =
                (FnCall(fun_name, [dest;src;lsize';alignment';volatile']), arr_et) in
              codegen_expr llcontext llmodule builder venv fenv tenv fn_call
          | ArrayComp(bt,lexpr,var_name,expr) ->
            let var_name' = make_ast fake_pos ("arrcomp" ^ (string_of_int !counter)) in
            let maybe_label = make_ast fake_pos (Fixed Unknown) in
            let mut = make_ast fake_pos Mut in
            let var_type = make_ast fake_pos (RefVT(bt,maybe_label,mut)) in
            let zeros = make_ast fake_pos (ArrayZeros lexpr) in
            let arr_dec = make_ast fake_pos (ArrayDec(var_name', var_type, zeros)) in
            ignore(codegen_stm llcontext llmodule builder None venv fenv tenv arr_dec);
            (* Step 1 done *)
            let i32 = make_ast fake_pos (UInt 32) in
            let var_type' = make_ast fake_pos (RefVT(i32, maybe_label, mut)) in
            let base_type = BaseET(i32, maybe_label) in
            let expr = make_ast fake_pos ((IntLiteral 0),base_type) in
            let base_dec = make_ast fake_pos (BaseDec(var_name, var_type',expr)) in
            ignore(codegen_stm llcontext llmodule builder None venv fenv tenv base_dec);
            (* Step 2 done *)
            (* Maybe try transforming into a for loop here? *)
            let iter_type = make_ast fake_pos (UInt 32) in
            let label = make_ast fake_pos (Fixed Unknown) in
            let low = make_ast fake_pos ((IntLiteral 0), (BaseET(iter_type,label))) in
            let high = make_ast fake_pos ((IntLiteral (size_of_lexpr lexpr.data)), (BaseET(iter_type,label))) in            
            let index = make_ast fake_pos (Variable(var_name), (BaseET(iter_type,label))) in
            let assignment = make_ast fake_pos (ArrayAssign(var_name', index, expr)) in
            let block = Env.new_env (), [assignment] in
            let body = make_ast fake_pos (For(var_name, iter_type, low, high, block)) in
            ignore(codegen_stm llcontext llmodule builder None venv fenv tenv body);
            (* Step 3, 4, and 5 done. These are all accomplished with the loop*)
            raise CodegenError
            (* TODO: What is the return value of a loop comprehension?
                     That should be returned here. Intuition says pointer to the list? *)
            (*
              1. Initialize array of type `bt to size `lexpr with zeros
              2. Initialize `var_name to zero
              3. Create a block containing expr
              4. At end of block, set array[`var_name] to |block|
              5. If |`var_name| >= |`lexpr| stop, otherwise, increment `var_name and jump to beginning of block
            
            *)
      end
    | ByRef r ->
      let var = find_var venv r in
      build_load var "argref" builder

and codegen_array_expr llcontext llmodule builder venv fenv = function
  | ArrayZeros lexpr ->
    begin
      match lexpr.data with
        | LIntLiteral n -> raise CodegenError
        | LUnspecified  -> raise CodegenError
    end
  | ArrayCopy var_name -> raise CodegenError
  | ArrayView(var_name, lexpr, expr) -> raise CodegenError
  | ArrayComp(bt,lexpr, var_name, expr) -> raise CodegenError

and codegen_expr llcontext llmodule builder venv fenv tenv = function
  | True, ty -> const_all_ones (expr_ty_to_llvm_ty llcontext ty)
  | False, ty -> const_null (expr_ty_to_llvm_ty llcontext ty)
  | IntLiteral i, ty ->
    const_int (expr_ty_to_llvm_ty llcontext ty) i
  | Variable var_name, ty ->
    (* TODO: We should probably check the lltype of ty and compare it to the type_of of the
             result? This would be a sanity check. Or maybe even better, we should cast
             the result to ty. This should pass the typechecker so it should be safe no matter
             what.*)
    find_var venv var_name
  | ArrayGet(var_name,expr), ty ->
    let arr = find_var venv var_name in
    let expr',expr_ty' = expr.data in
    begin
    match expr_to_ty expr' with
      | BaseET({data=(UInt 32)},l) as ty' -> (* TODO: Check that this is the type of an index *) 
        let index = codegen_expr llcontext llmodule builder venv fenv tenv (expr', ty') in
        let p = build_gep arr [| const_int (i32_type llcontext) 0; index |] "ptr" builder in
        build_load p (var_name.data ^ "_arrget") builder
      | _ -> raise CodegenError
    end
  | ArrayLen(var_name), ty ->
    let ty' = type_of (find_var venv var_name) in
    let arr_len = array_length ty' in
    codegen_expr llcontext llmodule builder venv fenv tenv ((IntLiteral arr_len), ty)
  | IntCast(base_ty,expr), ty ->
    let v = codegen_expr llcontext llmodule builder venv fenv tenv expr.data in
    build_cast llcontext v base_ty.data
  | UnOp(op,expr), ty ->
    let expr' = codegen_expr llcontext llmodule builder venv fenv tenv expr.data in
    codegen_unop builder expr' op
  | BinOp(op,expr1,expr2), ty ->
    let ty' = match ty with
      | BaseET(bt,_) -> bt
      | ArrayET _ -> raise CodegenError in
    let e1 = codegen_expr llcontext llmodule builder venv fenv tenv expr1.data in
    let e2 = codegen_expr llcontext llmodule builder venv fenv tenv expr2.data in
    codegen_binop op e1 e2 ty'.data builder
  | TernOp(expr1,expr2,expr3), ty -> raise CodegenError
  | FnCall(fun_name,args), ty ->
    let fun_dec = get_fn fenv fun_name in
    let callee = match lookup_function fun_name.data llmodule with
      | Some fn -> fn
      | None -> raise CodegenError in
    let codegen_arg' = codegen_arg llcontext llmodule builder venv fenv tenv in
    let args' = List.map2 codegen_arg' args fun_dec.args in
    build_call callee (Array.of_list args') "calltmp" builder
  | Declassify(expr), ty -> raise CodegenError
    (* TODO: We should never do anything with declassify. That is simply for the typechecker? *)

and extend_to ctx builder signed ty v =
  let llvm_ty = expr_ty_to_llvm_ty ctx ty in
  let lb,rb = integer_bitwidth llvm_ty, integer_bitwidth (type_of v) in
  match lb,rb with
    | lb,rb when lb < rb -> build_trunc v llvm_ty "trunctmp" builder
    | lb,rb when lb > rb && signed -> build_sext v llvm_ty "extendtmp" builder
    | lb,rb when lb > rb -> build_zext v llvm_ty "extendtmp" builder
    | _ -> v

and codegen_ext llcontext llmodule builder venv fenv tenv ty (expr : expr) =
  match expr.data with
    | expr',ty' ->
      let expr' = codegen_expr llcontext llmodule builder venv fenv tenv expr.data in
      extend_to llcontext builder (is_signed ty) ty' expr'

and vt_to_bt = function
  | RefVT(bt,_,_) -> bt.data
  | ArrayVT _ -> raise CodegenError

and codegen_stm llcontext llmodule builder ret_ty venv fenv tenv = function
  | {data=BaseDec(var_name,var_type,expr)} ->
    let v = find_var venv var_name in
    let expr' = codegen_ext llcontext llmodule builder venv fenv tenv (vt_to_bt var_type.data) expr in
    ignore(build_store expr' v builder)
  | {data=ArrayDec(var_name,var_type,arr_expr)} ->
    (* TODO: Gotta do some rearranging here *)
    raise CodegenError
  | {data=BaseAssign(var_name,expr)} ->
    let v = find_var venv var_name in
    let _,ty = expr.data in
    let ty' = (expr_ty_to_base_ty ty).data in
    let expr' = codegen_ext llcontext llmodule builder venv fenv tenv ty' expr in
    let var = build_load v var_name.data builder in
    ignore(build_store expr' var builder)
  | {data=ArrayAssign(var_name,array_index,expr)} ->
    let v = find_var venv var_name in
    let index = codegen_expr llcontext llmodule builder venv fenv tenv array_index.data in
    let expr' = codegen_expr llcontext llmodule builder venv fenv tenv expr.data in
    let p = build_gep v [| const_int (i32_type llcontext) 0; index|] "ptr" builder in
    ignore(build_store expr' p builder)
  | {data=If(cond,thenstms,elsestms)} ->
    
    let cond' = codegen_expr llcontext llmodule builder venv fenv tenv cond.data in
    let one = const_int (i32_type llcontext) 1 in
    let cond_val = build_icmp Icmp.Eq cond' one "branchcompare" builder in

    let start_bb = insertion_block builder in
    let parent_function = block_parent start_bb in

    let then_bb = append_block llcontext "thenbranch" parent_function in
    position_at_end then_bb builder;
    codegen_stms llcontext llmodule builder ret_ty venv fenv tenv thenstms;

    let new_then_bb = insertion_block builder in


    let else_bb = append_block llcontext "elsebranch" parent_function in
    position_at_end else_bb builder;
    codegen_stms llcontext llmodule builder ret_ty venv fenv tenv elsestms;

    let new_else_bb = insertion_block builder in

    let merge_bb = append_block llcontext "branchmerge" parent_function in
    position_at_end merge_bb builder;

    position_at_end start_bb builder;
    ignore(build_cond_br cond_val then_bb else_bb builder);

    position_at_end new_then_bb builder;
    ignore(build_br merge_bb builder);
    position_at_end new_else_bb builder;
    ignore(build_br merge_bb builder);
    position_at_end merge_bb builder
  | {data=For(var_name,base_type,low_expr,high_expr,statements)} ->
    let preheader = insertion_block builder in
    let parent_function = block_parent preheader in
    let bb_check = append_block llcontext "loop_check" parent_function in
    let bb_body = append_block llcontext "loop_body" parent_function in
    let bb_end = append_block llcontext "loop_end" parent_function in
    let i = find_var venv var_name in
    let low = codegen_ext llcontext llmodule builder venv fenv tenv base_type.data low_expr in
    ignore(build_store low i builder);
    ignore(build_br bb_check builder);
    position_at_end bb_check builder;
    let i' = build_load i var_name.data builder in
    let high = codegen_ext llcontext llmodule builder venv fenv tenv base_type.data high_expr in
    let cmp = if is_signed base_type.data then Icmp.Slt else Icmp.Ult in
    let cond = build_icmp cmp i' high "loopcond" builder in
    ignore(build_cond_br cond bb_body bb_end builder);
    position_at_end bb_body builder;
    codegen_stms llcontext llmodule builder ret_ty venv fenv tenv statements;
    let i'' = build_load i var_name.data builder in
    let one = (const_int (bt_to_llvm_ty llcontext base_type.data) 1) in
    let incr = build_add i'' one "loopincr" builder in
    ignore(build_store incr i builder);
    ignore(build_br bb_check builder);
    position_at_end bb_end builder
  | {data=VoidFnCall(fun_name,arg_exprs)} ->
    (* TODO: refactor this with FnCall *)
    let fun_dec = get_fn fenv fun_name in
    let callee = match lookup_function fun_name.data llmodule with
      | Some fn -> fn
      | None -> raise CodegenError in
    let codegen_arg' = codegen_arg llcontext llmodule builder venv fenv tenv in
    let args' = List.map2 codegen_arg' arg_exprs fun_dec.args in
    ignore(build_call callee (Array.of_list args') "voidcalltmp" builder)
  | {data=VoidReturn} ->
    ignore (build_ret_void builder)
  | {data=Return(expr)} ->
    match ret_ty with
      | Some BaseET(bt,label) ->
        let ret' = codegen_ext llcontext llmodule builder venv fenv tenv bt.data expr in
        ignore(build_ret ret' builder)
        (* TODO: assert valid function here *)
      | Some ArrayET _ -> raise CodegenError (* TODO: Cannot retur an array yet *)
      | None -> raise CodegenError

and codegen_stms llcontext llmodule builder ret_ty venv fenv tenv (stms : Tast.block) =
  let _,stms' = stms in
  let venv' = Env.new_env () in
  ignore(List.map (codegen_stm llcontext llmodule builder ret_ty venv' fenv tenv) stms')

let codegen_fun llcontext llmodule builder = function
  | { data=FunDec(name,ret,params,body) } ->
    Log.info "Generating function, %s" name.data;
    let param_types = List.map (param_to_type llcontext) params in
    let param_types' = Array.of_list param_types in
    let ret_ty = get_ret_ty llcontext ret in
    let ft = function_type ret_ty param_types' in
    let ft' =
      match lookup_function name.data llmodule with
        | None -> declare_function name.data ft llmodule
        | Some f -> raise FunctionAlreadyDefined in
    let bb = append_block llcontext "entry" ft' in
    position_at_end bb builder;
    let lvar_env = Env.new_env () in
    let tenv = Env.new_env () in
    let fenv = new_fenv () in
    (*Env.add_var fenv name ft';*)
    allocate_args llcontext builder lvar_env params;
    allocate_stack llcontext builder lvar_env body;
    (* TODO: store_args?? *)
    match ret with
      | None -> raise CodegenError (* TODO: Void is not implemented yet *)
      | Some ret' ->
        codegen_stms llcontext llmodule builder (Some ret'.data) lvar_env fenv tenv body;
    (*let ret' = codegen_ext llcontext llmodule builder var_env ret in
    build_ret ret' builder;*)
    Llvm_analysis.assert_valid_function ft';
    ft'

let rec codegen_fdecs llcontext llmodule builder = function
  | [] -> ()
  | fd::rest ->
    ignore(codegen_fun llcontext llmodule builder fd);
    codegen_fdecs llcontext llmodule builder rest

let rec codegen llcontext llmodule builder = function
  | Module(_,fdecs) ->
    Log.info "Codegening module";
    codegen_fdecs llcontext llmodule builder fdecs