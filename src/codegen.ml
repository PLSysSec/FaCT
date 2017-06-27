open Tast
open Llvm
open Pos
open Env

exception CodegenError
exception FunctionAlreadyDefined

let is_signed = function
  | UInt _ -> false
  | Int _  -> true
  | Bool   -> raise CodegenError
  | Num _  -> raise CodegenError

let get_size = function
  | LIntLiteral s      -> s
  | LVariable var_name -> raise CodegenError
  | LLength var_name   -> raise CodegenError

let bt_to_llvm_ty ctx = function
  | UInt size when size <= 8  -> i8_type ctx
  | UInt size when size <= 16 -> i16_type ctx
  | UInt size when size <= 32 -> i32_type ctx
  | Int  size when size <= 8  -> i8_type ctx
  | Int  size when size <= 16 -> i16_type ctx
  | Int  size when size <= 32 -> i32_type ctx
  | Bool                      -> i32_type ctx (* TODO: Double check this*)
  | Num(i,b)                  -> raise CodegenError

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
  in
  ignore(List.map allocate_stack' stms)

let codegen_expr llcontext llmodule builder = function
  | True, ty -> raise CodegenError
  | False, ty -> raise CodegenError
  | IntLiteral i, ty -> raise CodegenError
  | Variable var_name, ty -> raise CodegenError
  | ArrayGet(var_name,expr), ty -> raise CodegenError
  | ArrayLen(var_name), ty -> raise CodegenError
  | IntCast(base_ty,expr), ty -> raise CodegenError
  | UnOp(op,expr), ty -> raise CodegenError
  | BinOp(op,expr1,expr2), ty -> raise CodegenError
  | TernOp(expr1,expr2,expr3), ty -> raise CodegenError
  | FnCall(fun_name,args), ty -> raise CodegenError
  | Declassify(expr), ty -> raise CodegenError

let rec codegen_stms llcontext llmodule builder stms =
  let rec codegen_stm = function
    | {data=BaseDec(var_name,var_type,expr)} -> raise CodegenError
    | {data=ArrayDec(var_name,var_type,arr_expr)} -> raise CodegenError
    | {data=BaseAssign(var_name,expr)} -> raise CodegenError
    | {data=ArrayAssign(var_name,array_index,expr)} -> raise CodegenError
    | {data=If(cond,thenstms,elsestms)} -> raise CodegenError
    | {data=For(var_name,base_type,low_expr,high_expr,statements)} -> raise CodegenError
    | {data=VoidFnCall(fun_name,arg_exprs)} -> raise CodegenError
    | {data=Return(expr)} -> raise CodegenError
  in
  ignore(List.map codegen_stm stms)

let extend_to ctx builder signed ty v =
  let llvm_ty = vt_to_llvm_ty ctx ty in
  let lb,rb = integer_bitwidth llvm_ty, integer_bitwidth (type_of v) in
  match lb,rb with
    | lb,rb when lb < rb -> build_trunc v llvm_ty "trunctmp" builder
    | lb,rb when lb > rb && signed -> build_sext v llvm_ty "extendtmp" builder
    | lb,rb when lb > rb -> build_zext v llvm_ty "extendtmp" builder
    | _ -> v

let codegen_ext llcontext llmodule builder venv ty expr =
  match expr.data with
    | expr,ty' ->
      let expr' = codegen_expr llcontext llmodule builder venv expr in
      extend_to llcontext builder (is_signed ty') ty expr'

let codegen_fun llcontext llmodule builder = function
  | { data=FunDec(name,ret,params,body) } ->
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
    let var_env = Env.new_env () in
    allocate_args llcontext builder var_env params;
    allocate_stack llcontext builder var_env body;
    (* TODO: store_args?? *)
    codegen_stms llcontext llmodule builder body;
    (*let ret' = codegen_ext llcontext llmodule builder var_env ret in
    build_ret ret' builder;*)
    Llvm_analysis.assert_valid_function ft';
    ()

let rec codegen_fdecs llcontext llmodule builder = function
  | [] -> ()
  | fd::rest -> codegen_fdecs llcontext llmodule builder rest

let rec codegen llcontext llmodule builder = function
  | Module(fdecs) ->
    codegen_fdecs llcontext llmodule builder fdecs