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

type fenv = (string,fentry) Hashtbl.t [@printer pp_hashtbl]
[@@deriving show]

let new_fenv () = Hashtbl.create 10

let has_fn = Hashtbl.mem

let get_fn fenv f =
  try
    Hashtbl.find fenv f.data
  with
    Not_found -> raise @@ Err.errFnNotDefined f

let add_fn = Hashtbl.add

(* End env functionality *)

(* TODO make this better *)
type renv = (string,llvalue) Hashtbl.t [@printer pp_hashtbl]
[@@deriving show]
let new_renv () = Hashtbl.create 10
let add_reg = Hashtbl.add
let get_reg renv r =
  try
    Hashtbl.find renv r
  with
    Not_found -> raise CodegenError
(* End reg env functionality *)

type codegen_ctx_record = {
  llcontext : llcontext;
  llmodule  : llmodule;
  builder   : llbuilder;
  venv      : llvalue env;
  fenv      : fenv;
  tenv      : variable_type env;
  renv      : renv;
}

type intrinsic = 
  | Memcpy

let string_of_intrinsic = function
  | Memcpy -> "llvm.memcpy.p0i8.p0i8.i64"

let declare_intrinsic cg_ctx = function
  | Memcpy ->
    let i32_ty = i32_type cg_ctx.llcontext in
    let i64_ty = i64_type cg_ctx.llcontext in
    let ptr_ty = pointer_type (i8_type cg_ctx.llcontext) in
    let bool_ty = i1_type cg_ctx.llcontext in
    let arg_types = [| ptr_ty; ptr_ty; i64_ty; i32_ty; bool_ty |] in
    let vt = void_type cg_ctx.llcontext in
    let ft = function_type vt arg_types in
    declare_function (string_of_intrinsic Memcpy) ft cg_ctx.llmodule

let get_intrinsic intrinsic cg_ctx =
  match lookup_function (string_of_intrinsic intrinsic) cg_ctx.llmodule with
    | Some fn -> fn
    | None -> declare_intrinsic cg_ctx intrinsic

let is_signed = function
  | UInt _   -> false
  | Int _    -> true
  | Bool     -> false
  | Num(i,s) -> s

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

(* Used to get the base type for arrays *)
let expr_ty_to_base_ty = function
  | BaseET({data=base_type},_) -> base_type
  | ArrayET({data=ArrayAT(bt,size)},_,_) -> bt.data

let expr_ty_to_llvm_ty ctx = function
  | BaseET({data=base_type},_) ->
    bt_to_llvm_ty ctx base_type
  | ArrayET({data=ArrayAT(at,size)},_,_) ->
    let arr_ty = bt_to_llvm_ty ctx at.data in
    let size = get_size size.data in
    array_type arr_ty size (* TODO: Double check that this is not a pointer*)

let rec byte_size_of_expr_ty = function
  | BaseET({data=UInt(n)},_)  -> n / 8
  | BaseET({data=Int(n)},_)   -> n / 8
  | BaseET({data=Bool},_)     -> raise CodegenError
  | BaseET({data=Num(n,_)},_) -> raise CodegenError
  | ArrayET({data=ArrayAT(bt,_)},_,_) ->
    byte_size_of_expr_ty (BaseET(bt, (make_ast fake_pos (Fixed Unknown))))

let get_ret_ty ctx = function
  | None -> raise CodegenError
  | Some ty -> expr_ty_to_llvm_ty ctx ty.data

(* Allocate all of the args for a function *)
let allocate_args cg_ctx args =
  let allocate_arg ({data=Param(var_name,var_type)} as arg) =
    let llvm_ty = param_to_type cg_ctx.llcontext arg in
    let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in
    add_var cg_ctx.venv var_name alloca
  in
  List.iter allocate_arg args

(* Allocate space for each variable declared inside a function. This is
   done at the beginning of the function. *)
(* NOTE(src): we really should find a way to get this directly from the tast venvs
   instead of descending through the AST again *)
let rec allocate_stack cg_ctx stms =
  let rec allocate_inject = function
    | {data=(Inject(_,stms),_)} -> List.iter allocate_stack' stms
    | _ -> ()
  and allocate_stack' = function
    | {data=BaseDec(var_name,var_type,expr)} ->
      allocate_inject expr;
      let llvm_ty = vt_to_llvm_ty cg_ctx.llcontext var_type.data in
      let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in
        add_var cg_ctx.venv var_name alloca
    | {data=ArrayDec(var_name,var_type,expr)} -> ()
      (*let llvm_ty = vt_to_llvm_ty cg_ctx.llcontext var_type.data in*)
      (* TODO: I think this will fail. I think this will just allocate a
         pointer. But we want it to allocate the space for the array.
         So yea, fix dis *)
      (*let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in
      add_var cg_ctx.venv var_name alloca*)
    | {data=BaseAssign(var_name,expr)} -> allocate_inject expr
    | {data=RegAssign(reg_name,expr)} -> allocate_inject expr
    | {data=ArrayAssign(var_name,index,expr)} ->
      allocate_inject index;
      allocate_inject expr
    | {data=Block(stms)} ->
      allocate_stack cg_ctx stms
    | {data=If(cond,thenstms,elsestms)} ->
      allocate_inject cond;
      allocate_stack cg_ctx thenstms;
      allocate_stack cg_ctx elsestms
    | {data=For(var_name,base_type,low,high,stms)} ->
      allocate_inject low;
      allocate_inject high;
      let llvm_ty = bt_to_llvm_ty cg_ctx.llcontext base_type.data in
      let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in
      (* TODO: Fix the scoping here. This will force us to use a different
         var name for the loop iterator for each for loop per function. We
         probably want to be able to reuse this?? *)
      add_var cg_ctx.venv var_name alloca;
      allocate_stack cg_ctx stms
    | {data=Return(expr)} -> allocate_inject expr
    | {data=VoidFnCall _} -> ()
    | {data=s} -> print_endline @@ show_statement' s; raise CodegenError
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
    | Ast.LogicalAnd -> build_and e1 e2 "landtmp" b
    | Ast.LogicalOr -> build_or e1 e2 "lortmp" b
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

let build_cast ctx b value = (* from, to *) function
  | Bool -> build_intcast value (bt_to_llvm_ty ctx Bool) "cast" b
  | UInt(n) -> build_intcast value (bt_to_llvm_ty ctx (UInt n)) "cast" b
  | Int(n) -> build_intcast value (bt_to_llvm_ty ctx (Int n)) "cast" b
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

let rec codegen_arg cg_ctx arg ty =
  match arg.data with
    | ByValue expr -> codegen_expr cg_ctx expr.data
    | ByArray arr ->
      begin
        let (arrdata,_) = arr.data in
        match arrdata with
          | ArrayZeros lexpr ->
            begin
              match lexpr.data with
                | LIntLiteral n ->
                  let llty = param_ty_to_llvm_ty ty in
                  let zero = const_int llty 0 in
                  let zeros = Array.make n zero in
                  let arr_ty = array_type llty n in
                  let arr = const_array arr_ty zeros in
                  build_array_alloca arr_ty arr "zerodarray" cg_ctx.builder
                | LUnspecified -> raise CodegenError

            end
          | ArrayCopy var_name ->
            let arr = find_var cg_ctx.venv var_name in
            let arr_ty = type_of arr in
            let arr_load = build_load arr "arrcopyload" cg_ctx.builder in
            build_array_alloca arr_ty arr_load "arrcopy" cg_ctx.builder (* Probs gonna segfault *)
          | ArrayView(var_name, expr, lexpr) ->
            raise CodegenError
            (* TODO: I think the transformations should do the heavy lifting here and the
                     TAST should get rid of ArrayView *)
            (*let arr = find_var cg_ctx.venv var_name in
            let arr_load = build_load arr "arrviewload" cg_ctx.builder in
            let _ = match lookup_function "memset" cg_ctx.llmodule with
              | Some fn -> fn
              | None ->
                let i32_ty = i32_type cg_ctx.llcontext in
                let ptr_ty = pointer_type (i8_type cg_ctx.llcontext) in
                let bool_ty = i1_type cg_ctx.llcontext in
                let arg_types = [| ptr_ty; ptr_ty; i32_ty; i32_ty; bool_ty |] in
                let memcpy = "llvm.memcpy.p0i8.i32" in
                declare_function memcpy (function_type (void_type cg_ctx.llcontext) arg_types) cg_ctx.llmodule in
              let dest_name = make_ast fake_pos ("dest" ^ (string_of_int !counter)) in
              ignore(counter := !counter + 1);
              let arr_ty = find_var cg_ctx.tenv var_name in
              let size = make_ast fake_pos (LIntLiteral(array_length (type_of arr))) in
              let zeros = make_ast fake_pos (ArrayZeros(size)) in
              let dest_ast = make_ast fake_pos (ArrayDec(dest_name, arr_ty, zeros)) in
              ignore(codegen_stm cg_ctx None dest_ast);
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
              codegen_expr cg_ctx fn_call*)
          | ArrayComp(bt,lexpr,var_name,expr) ->
            (*let var_name' = make_ast fake_pos ("arrcomp" ^ (string_of_int !counter)) in
            let maybe_label = make_ast fake_pos (Fixed Unknown) in
            let mut = make_ast fake_pos Mut in
            let var_type = make_ast fake_pos (RefVT(bt,maybe_label,mut)) in
            let zeros = make_ast fake_pos (ArrayZeros lexpr) in
            let arr_dec = make_ast fake_pos (ArrayDec(var_name', var_type, zeros)) in
            ignore(codegen_stm cg_ctx None arr_dec);
            (* Step 1 done *)
            let i32 = make_ast fake_pos (UInt 32) in
            let var_type' = make_ast fake_pos (RefVT(i32, maybe_label, mut)) in
            let base_type = BaseET(i32, maybe_label) in
            let expr = make_ast fake_pos ((IntLiteral 0),base_type) in
            let base_dec = make_ast fake_pos (BaseDec(var_name, var_type',expr)) in
            ignore(codegen_stm cg_ctx None base_dec);
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
            ignore(codegen_stm cg_ctx None body);*)
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
      let var = find_var cg_ctx.venv r in
      build_load var "argref" cg_ctx.builder

and codegen_expr cg_ctx = function
  | True, ty -> const_all_ones (expr_ty_to_llvm_ty cg_ctx.llcontext ty)
  | False, ty -> const_null (expr_ty_to_llvm_ty cg_ctx.llcontext ty)
  | IntLiteral i, ty -> const_int (expr_ty_to_llvm_ty cg_ctx.llcontext ty) i
  | Register reg_name, ty ->
    get_reg cg_ctx.renv reg_name
  | Variable var_name, ty ->
    Log.error "codegen var %s" var_name.data;
    (* TODO: We should probably check the lltype of ty and compare it to the type_of of the
             result? This would be a sanity check. Or maybe even better, we should cast
             the result to ty. This should pass the typechecker so it should be safe no matter
             what.*)
    let store = find_var cg_ctx.venv var_name in
    build_load store var_name.data cg_ctx.builder
  | ArrayGet(var_name,expr), ty ->
    let arr = find_var cg_ctx.venv var_name in
    let index = codegen_expr cg_ctx expr.data in
    let p = build_gep arr [| index; index |] "ptr" cg_ctx.builder in
    build_load p (var_name.data ^ "_arrget") cg_ctx.builder
  | ArrayLen(var_name), ty ->
    let ty' = type_of (find_var cg_ctx.venv var_name) in
    let arr_len = array_length ty' in
    codegen_expr cg_ctx ((IntLiteral arr_len), ty)
  | IntCast(base_ty,expr), ty ->
    let v = codegen_expr cg_ctx expr.data in
    build_cast cg_ctx.llcontext cg_ctx.builder v base_ty.data
  | UnOp(op,expr), ty ->
    let expr' = codegen_expr cg_ctx expr.data in
    codegen_unop cg_ctx.builder expr' op
  | BinOp(op,expr1,expr2), ty ->
    let ty' = match ty with
      | BaseET(bt,_) -> bt
      | ArrayET _ -> raise CodegenError in
    let e1 = codegen_expr cg_ctx expr1.data in
    let e2 = codegen_expr cg_ctx expr2.data in
    codegen_binop op e1 e2 ty'.data cg_ctx.builder
  | TernOp(expr1,expr2,expr3), ty ->
    let e1 = codegen_expr cg_ctx expr1.data in
    let e1' = build_is_not_null e1 "condtmp" cg_ctx.builder in
    let e2 = codegen_expr cg_ctx expr2.data in
    let e3 = codegen_expr cg_ctx expr3.data in
      build_select e1' e2 e3 "terntmp" cg_ctx.builder
  | FnCall(fun_name,args), ty ->
    let fun_dec = get_fn cg_ctx.fenv fun_name in
    let callee = match lookup_function fun_name.data cg_ctx.llmodule with
      | Some fn -> fn
      | None -> raise CodegenError in
    let codegen_arg' = codegen_arg cg_ctx in
    let args' = List.map2 codegen_arg' args fun_dec.args in
    build_call callee (Array.of_list args') "calltmp" cg_ctx.builder
  | Declassify(expr), ty -> codegen_expr cg_ctx expr.data
  | Select(expr1,expr2,expr3), ty ->
    (* XXX this needs to be a constant time select *)
    let e1 = codegen_expr cg_ctx expr1.data in
    let e1' = build_is_not_null e1 "condtmp" cg_ctx.builder in
    let e2 = codegen_expr cg_ctx expr2.data in
    let e3 = codegen_expr cg_ctx expr3.data in
      build_select e1' e2 e3 "terntmp" cg_ctx.builder
  | Inject(reg,stms), ty ->
    let ret_ty = None in
      ignore(List.map (codegen_stm cg_ctx ret_ty) stms);
      get_reg cg_ctx.renv reg
  | e, ty -> print_endline @@ show_expr' e; raise CodegenError

and extend_to ctx builder signed ty v =
  let llvm_ty = expr_ty_to_llvm_ty ctx ty in
  let lb,rb = integer_bitwidth llvm_ty, integer_bitwidth (type_of v) in
  match lb,rb with
    | lb,rb when lb < rb -> build_trunc v llvm_ty "trunctmp" builder
    | lb,rb when lb > rb && signed -> build_sext v llvm_ty "extendtmp" builder
    | lb,rb when lb > rb -> build_zext v llvm_ty "extendtmp" builder
    | _ -> v

and codegen_ext cg_ctx ty (expr : expr) =
  match expr.data with
    | expr',ty' ->
      let expr' = codegen_expr cg_ctx expr.data in
      extend_to cg_ctx.llcontext cg_ctx.builder (is_signed ty) ty' expr'

and vt_to_bt = function
  | RefVT(bt,_,_) -> bt.data
  | ArrayVT _ -> raise CodegenError

and codegen_array_expr cg_ctx = function
  (* XXX gary here too pls *)
  | ArrayLit exprs,ty ->
    (* TODO: This needs optimization. We want this array to be global if
             all exprs are known at compile time. Side note -- this is
             what clang does.*)
    let exprs' = List.map (fun expr -> expr.data) exprs in
    let ll_exprs = List.map (codegen_expr cg_ctx) exprs' in
    let ty' = expr_ty_to_base_ty ty in
    let ll_ty = bt_to_llvm_ty cg_ctx.llcontext ty' in
    let arr_ty = array_type ll_ty (List.length ll_exprs) in
    let zero = const_int ll_ty 0 in
    let alloca = build_array_alloca arr_ty zero "arraylit" cg_ctx.builder in
    let gep i el =
      let i' = const_int (i32_type cg_ctx.llcontext) i in
      let ptr = build_in_bounds_gep alloca [| i'; i' |]  "index" cg_ctx.builder in
      build_store el ptr cg_ctx.builder |> ignore
      in
    List.iteri gep ll_exprs;
    alloca
  | ArrayZeros lexpr,ty ->
    begin
      match lexpr.data with
        | LIntLiteral n ->
          let ty' = expr_ty_to_base_ty ty in
          let ll_ty = bt_to_llvm_ty cg_ctx.llcontext ty' in
          let zero = const_int ll_ty 0 in
          let zeros = Array.make n zero in
          let arr_ty = array_type ll_ty n in
          let alloca = build_array_alloca arr_ty zero "zerodarray" cg_ctx.builder in
          build_store (const_array ll_ty zeros) alloca cg_ctx.builder |> ignore;
          alloca
        | LUnspecified  -> raise CodegenError
    end
  | ArrayCopy var_name,ty ->
    let ll_ty = expr_ty_to_llvm_ty cg_ctx.llcontext ty in
    let zero' = const_int (i32_type cg_ctx.llcontext) 0 in
    let alloca = build_array_alloca ll_ty zero' "copiedarray" cg_ctx.builder in
    let from = find_var cg_ctx.venv var_name in
    let cpy_len = array_length ll_ty in
    let num_bytes = (byte_size_of_expr_ty ty) * cpy_len in
    let ll_cpy_len = (const_int (i64_type cg_ctx.llcontext) num_bytes) in
    let alignment = (const_int (i32_type cg_ctx.llcontext) 16) in
    let volatility = (const_int (i1_type cg_ctx.llcontext) 0) in
    let source_gep = build_in_bounds_gep from [| zero'; zero' |] "source_gep" cg_ctx.builder in
    let dest_gep   = build_in_bounds_gep alloca [| zero'; zero' |] "dest_gep" cg_ctx.builder in
    let source_cast_ty = pointer_type (i8_type cg_ctx.llcontext) in
    let source_casted = build_bitcast source_gep source_cast_ty "source_casted" cg_ctx.builder in
    let dest_casted = build_bitcast dest_gep source_cast_ty "dest_cast" cg_ctx.builder in
    let args = [| source_casted; dest_casted; ll_cpy_len; alignment; volatility |] in
    let memcpy = get_intrinsic Memcpy cg_ctx in
    build_call memcpy args "" cg_ctx.builder |> ignore;
    alloca
  | ArrayView(var_name, lexpr, expr),ty -> raise CodegenError
  | ArrayComp(bt,lexpr, var_name, expr),ty -> raise CodegenError

and codegen_stm cg_ctx ret_ty = function
  | {data=BaseDec(var_name,var_type,expr)} ->
    let v = find_var cg_ctx.venv var_name in
    let expr' = codegen_ext cg_ctx (vt_to_bt var_type.data) expr in
    ignore(build_store expr' v cg_ctx.builder)
  | {data=ArrayDec(var_name,var_type,arr_expr)} ->
    let alloca = codegen_array_expr cg_ctx arr_expr.data in
    add_var cg_ctx.venv var_name alloca
  | {data=BaseAssign(var_name,expr)} ->
    let v = find_var cg_ctx.venv var_name in
    let _,ty = expr.data in
    let ty' = expr_ty_to_base_ty ty in
    let expr' = codegen_ext cg_ctx ty' expr in
    ignore(build_store expr' v cg_ctx.builder)
  | {data=RegAssign(reg_name,expr)} ->
    let expr' = codegen_expr cg_ctx expr.data in
      add_reg cg_ctx.renv reg_name expr';
  | {data=ArrayAssign(var_name,array_index,expr)} ->
    let v = find_var cg_ctx.venv var_name in
    let index = codegen_expr cg_ctx array_index.data in
    let expr' = codegen_expr cg_ctx expr.data in
    let p = build_gep v [| const_int (i32_type cg_ctx.llcontext) 0; index|] "ptr" cg_ctx.builder in
    ignore(build_store expr' p cg_ctx.builder)
  | {data=Block(stms)} ->
    codegen_stms cg_ctx ret_ty stms
  | {data=If(cond,thenstms,elsestms)} ->

    let cond' = codegen_expr cg_ctx cond.data in
    let one = const_int (i32_type cg_ctx.llcontext) 1 in
    let cond_val = build_icmp Icmp.Eq cond' one "branchcompare" cg_ctx.builder in

    let start_bb = insertion_block cg_ctx.builder in
    let parent_function = block_parent start_bb in

    let then_bb = append_block cg_ctx.llcontext "thenbranch" parent_function in
    position_at_end then_bb cg_ctx.builder;
    codegen_stms cg_ctx ret_ty thenstms;

    let new_then_bb = insertion_block cg_ctx.builder in


    let else_bb = append_block cg_ctx.llcontext "elsebranch" parent_function in
    position_at_end else_bb cg_ctx.builder;
    codegen_stms cg_ctx ret_ty elsestms;

    let new_else_bb = insertion_block cg_ctx.builder in

    let merge_bb = append_block cg_ctx.llcontext "branchmerge" parent_function in
    position_at_end merge_bb cg_ctx.builder;

    position_at_end start_bb cg_ctx.builder;
    ignore(build_cond_br cond_val then_bb else_bb cg_ctx.builder);

    position_at_end new_then_bb cg_ctx.builder;
    ignore(build_br merge_bb cg_ctx.builder);
    position_at_end new_else_bb cg_ctx.builder;
    ignore(build_br merge_bb cg_ctx.builder);
    position_at_end merge_bb cg_ctx.builder
  | {data=For(var_name,base_type,low_expr,high_expr,statements)} ->
    let preheader = insertion_block cg_ctx.builder in
    let parent_function = block_parent preheader in
    let bb_check = append_block cg_ctx.llcontext "loop_check" parent_function in
    let bb_body = append_block cg_ctx.llcontext "loop_body" parent_function in
    let bb_end = append_block cg_ctx.llcontext "loop_end" parent_function in
    let i = find_var cg_ctx.venv var_name in
    let low = codegen_ext cg_ctx base_type.data low_expr in
    ignore(build_store low i cg_ctx.builder);
    ignore(build_br bb_check cg_ctx.builder);
    position_at_end bb_check cg_ctx.builder;
    let i' = build_load i var_name.data cg_ctx.builder in
    let high = codegen_ext cg_ctx base_type.data high_expr in
    let cmp = if is_signed base_type.data then Icmp.Slt else Icmp.Ult in
    let cond = build_icmp cmp i' high "loopcond" cg_ctx.builder in
    ignore(build_cond_br cond bb_body bb_end cg_ctx.builder);
    position_at_end bb_body cg_ctx.builder;
    codegen_stms cg_ctx ret_ty statements;
    let i'' = build_load i var_name.data cg_ctx.builder in
    let one = (const_int (bt_to_llvm_ty cg_ctx.llcontext base_type.data) 1) in
    let incr = build_add i'' one "loopincr" cg_ctx.builder in
    ignore(build_store incr i cg_ctx.builder);
    ignore(build_br bb_check cg_ctx.builder);
    position_at_end bb_end cg_ctx.builder
  | {data=VoidFnCall(fun_name,arg_exprs)} ->
    (* TODO: refactor this with FnCall *)
    let fun_dec = get_fn cg_ctx.fenv fun_name in
    let callee = match lookup_function fun_name.data cg_ctx.llmodule with
      | Some fn -> fn
      | None -> raise CodegenError in
    let codegen_arg' = codegen_arg cg_ctx in
    let args' = List.map2 codegen_arg' arg_exprs fun_dec.args in
    ignore(build_call callee (Array.of_list args') "voidcalltmp" cg_ctx.builder)
  | {data=VoidReturn} ->
    ignore (build_ret_void cg_ctx.builder)
  | {data=Return(expr)} ->
    begin
      match ret_ty with
        | Some BaseET(bt,label) ->
          let ret' = codegen_ext cg_ctx bt.data expr in
            ignore(build_ret ret' cg_ctx.builder)
        (* TODO: assert valid function here *)
        | Some ArrayET _ -> raise CodegenError (* TODO: Cannot retur an array yet *)
        | None -> raise CodegenError
    end
  | {data=s} -> print_endline @@ show_statement' s; raise CodegenError

and codegen_stms cg_ctx ret_ty (stms : Tast.block) =
  let _,stms' = stms in
  ignore(List.map (codegen_stm cg_ctx ret_ty) stms')

let codegen_fun llcontext llmodule builder fenv = function
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
    let venv = Env.new_env () in
    let tenv = Env.new_env () in
    let renv = new_renv () in
    let cg_ctx = { llcontext; llmodule; builder; venv; fenv; tenv; renv } in
    allocate_args cg_ctx params;
    allocate_stack cg_ctx body;
    (* TODO: store_args?? *)
    match ret with
      | None -> raise CodegenError (* TODO: Void is not implemented yet *)
      | Some ret' ->
        codegen_stms cg_ctx (Some ret'.data) body;
    (*let ret' = codegen_ext llcontext llmodule builder var_env ret in
    build_ret ret' builder;*)
    Llvm_analysis.assert_valid_function ft';
        let fentry = { ret_ty=ret; args=params } in
          Hashtbl.add fenv name.data fentry;
          ft'

let rec codegen_fdecs llcontext llmodule builder fenv = function
  | [] -> ()
  | fd::rest ->
    ignore(codegen_fun llcontext llmodule builder fenv fd);
    codegen_fdecs llcontext llmodule builder fenv rest

let rec codegen llcontext llmodule builder = function
  | Module(_,fdecs) ->
    Log.info "Codegening module";
    let fenv = new_fenv () in
      codegen_fdecs llcontext llmodule builder fenv fdecs
