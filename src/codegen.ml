open Tast
open Llvm
open Pos
open Env
open Ctverif

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

type codegen_ctx_record = {
  llcontext   : llcontext;
  llmodule    : llmodule;
  builder     : llbuilder;
  venv        : llvalue env;
  fenv        : fenv;
  tenv        : array_type env;
  vtenv       : variable_type env;
  verify_llvm : bool;
}

let mk_ctx llcontext llmodule builder venv fenv tenv vtenv verify_llvm =
  { llcontext; llmodule; builder; venv; fenv; tenv; vtenv; verify_llvm }

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

let get_size ctx = function
  | LIntLiteral s -> s
  | LDynamic var_name -> raise CodegenError

let bt_to_llvm_ty cg_ctx = function
  | UInt size when size <= 8  -> i8_type cg_ctx.llcontext
  | UInt size when size <= 16 -> i16_type cg_ctx.llcontext
  | UInt size when size <= 32 -> i32_type cg_ctx.llcontext
  | UInt  size when size <= 64 -> i64_type cg_ctx.llcontext
  | UInt  size when size <= 128 -> integer_type cg_ctx.llcontext 128
  | Int  size when size <= 8  -> i8_type cg_ctx.llcontext
  | Int  size when size <= 16 -> i16_type cg_ctx.llcontext
  | Int  size when size <= 32 -> i32_type cg_ctx.llcontext
  | Int  size when size <= 64 -> i64_type cg_ctx.llcontext
  | Bool                      -> i1_type cg_ctx.llcontext (* TODO: Double check this*)
  | Num(i,b)                  -> i32_type cg_ctx.llcontext (* TODO: Double check semantics for `Num` *)

let is_dynamic_sized_array = function
  | ArrayVT({data=ArrayAT(_,{data=LDynamic _})},_,_) -> true
  | _ -> false

let vt_to_llvm_ty (cg_ctx : codegen_ctx_record) = function
  | RefVT({data=base_type},maybe_label,_) ->
    bt_to_llvm_ty cg_ctx base_type
  | ArrayVT({data=ArrayAT(at,size)},maybe_label,_) ->
    let arr_ty = bt_to_llvm_ty cg_ctx at.data in
    let size = get_size cg_ctx size.data in
    (* TODO: Should we be passing arrays as a * or **? *)
    array_type arr_ty size

let param_to_type cg_ctx = function
  | {data=Param(var_name,
    {data=RefVT({data=base_type},maybe_label,{data=Mut})})} ->
    pointer_type(bt_to_llvm_ty cg_ctx base_type)
  | {data=Param(var_name,
    {data=RefVT({data=base_type},maybe_label,{data=Const})})} ->
    bt_to_llvm_ty cg_ctx base_type
  | {data=Param(var_name,{data=ArrayVT({data=ArrayAT(bt,size)} as ty,maybe_label,_)})} ->
    begin
      match size.data with
        | LIntLiteral s ->
          pointer_type(array_type (bt_to_llvm_ty cg_ctx bt.data) s)
        | LDynamic _ ->
          Hashtbl.add (get_vtbl cg_ctx.tenv) var_name.data ty;
          pointer_type (bt_to_llvm_ty cg_ctx bt.data)
    end

(* Used to get the base type for arrays *)
let expr_ty_to_base_ty = function
  | BaseET({data=base_type},_) -> base_type
  | ArrayET({data=ArrayAT(bt,size)},_,_) -> bt.data

let bitsize cg_ctx = function
  | BaseET({data=base_type},_) ->
    bt_to_llvm_ty cg_ctx base_type
  | ArrayET({data=ArrayAT(at,size)},_,_) ->
    bt_to_llvm_ty cg_ctx at.data

let expr_ty_to_llvm_ty cg_ctx = function
  | BaseET({data=base_type},_) ->
    bt_to_llvm_ty cg_ctx base_type
  | ArrayET({data=ArrayAT(at,size)},_,_) ->
    let arr_ty = bt_to_llvm_ty cg_ctx at.data in
    let size = get_size cg_ctx size.data in
    array_type arr_ty size (* TODO: Double check that this is not a pointer*)

let rec byte_size_of_expr_ty = function
  | BaseET({data=UInt(n)},_)  -> n / 8
  | BaseET({data=Int(n)},_)   -> n / 8
  | BaseET({data=Bool},_)     -> raise CodegenError
  | BaseET({data=Num(n,_)},_) -> raise CodegenError
  | ArrayET({data=ArrayAT(bt,_)},_,_) ->
    byte_size_of_expr_ty (BaseET(bt, (make_ast fake_pos (Fixed Unknown))))

let get_ret_ty cg_ctx = function
  | None -> void_type cg_ctx.llcontext
  | Some ty -> expr_ty_to_llvm_ty cg_ctx ty.data

(* Allocate all of the args for a function *)
let allocate_args cg_ctx args f =
  let allocate_arg ({data=Param(var_name,var_type)} as arg) ll_arg =
    let () =
    match var_type.data with
      | RefVT (bt,ml,{data=Mut}) ->
        add_var cg_ctx.venv var_name ll_arg
      | RefVT (bt,ml,{data=Const}) ->
        let ty = param_to_type cg_ctx arg in
        let alloca = build_alloca ty var_name.data cg_ctx.builder in
        add_var cg_ctx.venv var_name alloca;
        build_store ll_arg alloca cg_ctx.builder |> ignore
      | ArrayVT({data=ArrayAT(bt,{data=LIntLiteral(s)})},_,_) ->
        let ty = array_type (bt_to_llvm_ty cg_ctx bt.data) s in
        let size = const_int (i32_type cg_ctx.llcontext) s in
        let loaded = build_load ll_arg "loadedarrptr" cg_ctx.builder in
        let alloca = build_alloca ty var_name.data cg_ctx.builder in
        add_var cg_ctx.venv var_name ll_arg;
        ()
      | ArrayVT({data=ArrayAT(bt,{data=LDynamic(var_name')})},_,_) ->
        let ty = pointer_type(bt_to_llvm_ty cg_ctx bt.data) in
        let alloca = (build_alloca ty var_name'.data cg_ctx.builder) in
        add_var cg_ctx.venv var_name alloca;
        build_store ll_arg alloca cg_ctx.builder |> ignore in
    
    set_value_name var_name.data ll_arg;
    (*let llvm_ty = param_to_type cg_ctx arg in
    let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in*)
    add_var cg_ctx.vtenv var_name var_type
  in
  let ll_args = Array.to_list (params f) in
  List.iter2 allocate_arg args ll_args

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
      let llvm_ty = vt_to_llvm_ty cg_ctx var_type.data in
      let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in
      add_var cg_ctx.venv var_name alloca;
      add_var cg_ctx.vtenv var_name var_type
    | {data=ArrayDec(var_name,var_type,expr)} -> ()
      (*let llvm_ty = vt_to_llvm_ty cg_ctx.llcontext var_type.data in*)
      (* TODO: I think this will fail. I think this will just allocate a
         pointer. But we want it to allocate the space for the array.
         So yea, fix dis *)
      (*let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in
      add_var cg_ctx.venv var_name alloca*)
    | {data=BaseAssign(var_name,expr)} -> allocate_inject expr
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
      let llvm_ty = bt_to_llvm_ty cg_ctx base_type.data in
      let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in
      (* TODO: Fix the scoping here. This will force us to use a different
         var name for the loop iterator for each for loop per function. We
         probably want to be able to reuse this?? *)
      add_var cg_ctx.venv var_name alloca;
      allocate_stack cg_ctx stms
    | {data=Return(expr)} -> allocate_inject expr
    | {data=VoidFnCall _} -> ()
    | {data=VoidReturn} -> ()
  in
  let env,stms' = stms in
  ignore(List.map allocate_stack' stms')

let codegen_binop cg_ctx op e1 e2 ty b =
  let e1_width = integer_bitwidth (type_of e1) in
  let e2_width = integer_bitwidth (type_of e2) in
  let e1,e2 =
    match e1_width,e2_width with
      | w1,w2 when w1 < w2 ->
        let e1' = (build_sext e1 (type_of e2)) "lhssext" cg_ctx.builder in
        e1',e2
      | w1,w2 when w1 > w2 -> 
        let e2' = (build_sext e2 (type_of e1)) "rhssext" cg_ctx.builder in
        e1,e2'
      | w1,w2 -> e1,e2 in
  let res = 
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
      | Ast.RightShift -> build_lshr e1 e2 "lrshift" b in

  let ret_ty = bt_to_llvm_ty cg_ctx ty in
  let ret_width = integer_bitwidth ret_ty in
  let expr_width = integer_bitwidth (type_of res) in
  match ret_width,expr_width with
    | rw,ew when rw < ew -> build_trunc res ret_ty "truncbinop" cg_ctx.builder
    | rw,ew when rw > ew -> build_sext res ret_ty "sextbinop" cg_ctx.builder
    | rw,ew -> res

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
  | LDynamic x -> raise CodegenError

let rec codegen_arg cg_ctx arg ty =
  let is_dynamic_array = function
    | ArrayView _,_ -> true
    | ArrayVar var_name,_ ->
      let some_arg =
        try Some(find_var cg_ctx.tenv var_name) with
          | _ -> None in
      begin
      match some_arg with
        | None -> false
        | Some arg -> true
      end
    | _,ArrayET({data=ArrayAT(_,{data=LDynamic _})},_,_) -> true
    | _,ArrayET({data=ArrayAT(_,{data=LIntLiteral _})},_,_) -> false
    | _ -> raise CodegenError in
  let vt = 
    match ty.data with
      | Param(_,vt) -> vt.data in
  match arg.data with
    | ByValue expr -> codegen_ext cg_ctx (vt_to_llvm_ty cg_ctx vt) expr
    | ByArray(arr,_) ->
      begin
        match ty.data with
          | Param(name,{data=ArrayVT({data=ArrayAT(bt,lexpr)},_,_)}) ->
            let arr = 
            begin
              match lexpr.data, is_dynamic_array arr.data with
                | LIntLiteral s,false ->
                  let arr',_ = codegen_array_expr cg_ctx name arr.data in
                  build_load arr' "arr" cg_ctx.builder |> ignore;
                  arr'
                | LIntLiteral s,true ->
                  let arr',_ = codegen_array_expr cg_ctx name arr.data in
                  let arr_type = array_type (bt_to_llvm_ty cg_ctx bt.data) s in
                  let pt = pointer_type arr_type in
                  build_bitcast arr' pt "dyntostaticarr" cg_ctx.builder
                | LDynamic var_name,true ->
                  let arr',_ = codegen_array_expr cg_ctx name arr.data in
                  build_load arr' "loadeddynarrarg" cg_ctx.builder
                | LDynamic var_name,false ->
                  let arr',_ = codegen_array_expr cg_ctx name arr.data in
                  let ll_ty = bt_to_llvm_ty cg_ctx bt.data in
                  build_bitcast arr' (pointer_type ll_ty) "arrtoptr" cg_ctx.builder
            end in
            (*remove_var cg_ctx.tenv name;*)
            arr
          | _-> raise CodegenError
      end;
    | ByRef r ->
      let var = find_var cg_ctx.venv r in
      match vt with
        | RefVT(_,_,{data=Const})
        | ArrayVT(_,_,{data=Const}) -> build_load var "argref" cg_ctx.builder
        | RefVT(_,_,{data=Mut})
        | ArrayVT(_,_,{data=Mut}) -> var


and codegen_expr cg_ctx = function
  | True, ty -> const_all_ones (expr_ty_to_llvm_ty cg_ctx ty)
  | False, ty -> const_null (expr_ty_to_llvm_ty cg_ctx ty)
  | IntLiteral i, ty -> const_int (expr_ty_to_llvm_ty cg_ctx ty) i
  | Variable var_name, ty ->
    (* TODO: We should probably check the lltype of ty and compare it to the type_of of the
             result? This would be a sanity check. Or maybe even better, we should cast
             the result to ty. This should pass the typechecker so it should be safe no matter
             what.*)
    let store = find_var cg_ctx.venv var_name in
    build_load store var_name.data cg_ctx.builder
  | ArrayGet(var_name,expr), ty ->
    let arr = find_var cg_ctx.venv var_name in
    let index = codegen_expr cg_ctx expr.data in
    let some_arg =
      try Some(find_var cg_ctx.tenv var_name) with
        | _ -> None in
    let indices,ptr = match some_arg with
      | None ->
        let zero = const_int (type_of index) 0 in
        let indices = [| zero; index |] in
        indices,arr
      | Some arg ->
        let indices = [| index |] in
        let ptr = build_load arr "loadedarrptr" cg_ctx.builder in
        indices,ptr in
    let p = build_in_bounds_gep ptr indices "ptr" cg_ctx.builder in
    build_load p "arrget" cg_ctx.builder
  | IntCast(base_ty,expr), ty ->
    let v = codegen_expr cg_ctx expr.data in
    build_cast cg_ctx cg_ctx.builder v base_ty.data
  | UnOp(op,expr), ty ->
    let llty = expr_ty_to_llvm_ty cg_ctx ty in
    let expr' = codegen_ext cg_ctx llty expr in
    codegen_unop cg_ctx.builder expr' op
  | BinOp(op,expr1,expr2), ty ->
    let ty' = match ty with
      | BaseET(bt,_) -> bt
      | ArrayET _ -> raise CodegenError in
    let e1 = codegen_expr cg_ctx expr1.data in
    let e2 = codegen_expr cg_ctx expr2.data in
    codegen_binop cg_ctx op e1 e2 ty'.data cg_ctx.builder
  | TernOp(expr1,expr2,expr3), ty ->
    let e1 = codegen_expr cg_ctx expr1.data in
    let e1' = build_is_not_null e1 "condtmp" cg_ctx.builder in
    let ty = expr_ty_to_llvm_ty cg_ctx ty in
    let e2 = codegen_ext cg_ctx ty expr2 in
    let e3 = codegen_ext cg_ctx ty expr3 in
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
    let e1 = codegen_expr cg_ctx expr1.data in
    let e1' = build_is_not_null e1 "condtmp" cg_ctx.builder in
    let ty' = expr_ty_to_llvm_ty cg_ctx ty in
    let e2 = codegen_ext cg_ctx ty' expr2 in
    let e3 = codegen_ext cg_ctx ty' expr3 in
    build_select e1' e2 e3 "selecttmp" cg_ctx.builder
  | Inject(var_name,stms), ty ->
    let ret_ty = None in
    ignore(List.map (codegen_stm cg_ctx ret_ty) stms);
    let store = find_var cg_ctx.venv var_name in
    build_load store var_name.data cg_ctx.builder

and extend_to ctx builder signed dest et v =
  let llvm_et = expr_ty_to_llvm_ty ctx et in
  let lb,rb = integer_bitwidth dest, integer_bitwidth llvm_et in
  match lb,rb with
    | lb',rb' when lb' = rb' -> v
    | lb',rb' when lb' < rb' -> build_trunc v dest "trunctmp" ctx.builder
    | lb',rb' when (lb' > rb') && signed -> build_sext v dest "sexttmp" ctx.builder
    | lb',rb' when lb' > rb' -> build_zext v dest "zexttmp" ctx.builder
    | _ -> raise CodegenError (* This should never be hit *)

and codegen_ext cg_ctx dest (expr : expr) =
  match expr.data with
    | expr',ty' ->
      let expr' = codegen_expr cg_ctx expr.data in
      extend_to cg_ctx cg_ctx.builder true dest ty' expr'

and vt_to_bt = function
  | RefVT(bt,_,_) -> bt.data
  | ArrayVT _ -> raise CodegenError

and codegen_array_expr cg_ctx arr_name = function
  (* XXX gary here too pls *)
  | ArrayVar var_name,ty -> find_var cg_ctx.venv var_name,false
  | ArrayLit exprs,ty ->
    (* TODO: This needs optimization. We want this array to be global if
             all exprs are known at compile time. Side note -- this is
             what clang does.*)
    let bitsize = bitsize cg_ctx ty in
    let ll_exprs' = List.map (codegen_ext cg_ctx bitsize) exprs in
    let arr_ty = array_type bitsize (List.length ll_exprs') in
    let zero = const_int bitsize 0 in
    let alloca = build_alloca arr_ty "arraylit" cg_ctx.builder in
    let gep i el =
      let i' = const_int (i32_type cg_ctx.llcontext) i in
      let ptr = build_in_bounds_gep alloca [| zero; i' |]  "index" cg_ctx.builder in
      build_store el ptr cg_ctx.builder |> ignore
      in
    List.iteri gep ll_exprs';
    alloca,false
  | ArrayZeros lexpr,ty ->
    begin
      match lexpr.data with
        | LIntLiteral n ->
          let ty' = expr_ty_to_base_ty ty in
          let ll_ty = bt_to_llvm_ty cg_ctx ty' in
          let zero = const_int ll_ty 0 in
          let zeros = Array.make n zero in
          let arr_ty = array_type ll_ty n in
          let alloca = build_alloca arr_ty "zerodarray" cg_ctx.builder in
          build_store (const_array ll_ty zeros) alloca cg_ctx.builder |> ignore;
          alloca,false
        | LDynamic x -> raise CodegenError
    end
  | ArrayCopy var_name,ty ->
    let ll_ty = expr_ty_to_llvm_ty cg_ctx ty in
    let alloca = build_alloca ll_ty "copiedarray" cg_ctx.builder in
    let from = find_var cg_ctx.venv var_name in
    let cpy_len = array_length ll_ty in
    let num_bytes = (byte_size_of_expr_ty ty) * cpy_len in
    let ll_cpy_len = (const_int (i64_type cg_ctx.llcontext) num_bytes) in
    let alignment = (const_int (i32_type cg_ctx.llcontext) 0) in
    let volatility = (const_int (i1_type cg_ctx.llcontext) 0) in
    let source_cast_ty = pointer_type (i8_type cg_ctx.llcontext) in
    let source_casted = build_bitcast from source_cast_ty "source_casted" cg_ctx.builder in
    let dest_casted = build_bitcast alloca source_cast_ty "dest_cast" cg_ctx.builder in
    let args = [| dest_casted; source_casted; ll_cpy_len; alignment; volatility |] in
    let memcpy = get_intrinsic Memcpy cg_ctx in
    build_call memcpy args "" cg_ctx.builder |> ignore;
    alloca,false
  | ArrayView(var_name, expr, lexpr),ty ->
    let index = codegen_expr cg_ctx expr.data in
    let from = find_var cg_ctx.venv var_name in
    let bt_at_of_et = function
      | ArrayET({data=ArrayAT(bt,_)} as at,_,_) -> bt,at
      | _ -> raise CodegenError
      in
    let some_arg =
      try Some(find_var cg_ctx.tenv var_name) with
        | _-> None in
    let r = 
    begin
    match some_arg with
      | None ->
        let bt,at = bt_at_of_et ty in
        let ty' = pointer_type (bt_to_llvm_ty cg_ctx bt.data) in
        let alloca = build_alloca ty' "arrview" cg_ctx.builder in
        let zero = const_int (type_of index) 0 in
        let indices = [| zero; index |] in
        let source_gep = build_in_bounds_gep from indices "source_gep" cg_ctx.builder in
        build_store source_gep alloca cg_ctx.builder |> ignore;
        alloca,true
      | Some arg ->
        let indices = [| index |] in
        let ptr = build_load from "loadedviewptr" cg_ctx.builder in
        let source_gep = build_in_bounds_gep ptr indices "source_gep" cg_ctx.builder in
        let bt,at = bt_at_of_et ty in
        let ty' = pointer_type (bt_to_llvm_ty cg_ctx bt.data) in
        let alloca = build_alloca ty' "arrviewdyn" cg_ctx.builder in
        build_store source_gep alloca cg_ctx.builder |> ignore;
        alloca,true
    end in
    r
  | ArrayComp(bt,lexpr, var_name, expr),ty -> raise CodegenError

and codegen_stm cg_ctx ret_ty = function
  | {data=BaseDec(var_name,var_type,expr)} ->
    let v = find_var cg_ctx.venv var_name in
    let expr' = codegen_ext cg_ctx (vt_to_llvm_ty cg_ctx var_type.data) expr in
    let s = build_store expr' v cg_ctx.builder in
    codegen_dec cg_ctx.verify_llvm var_type v cg_ctx.llcontext cg_ctx.llmodule cg_ctx.builder;
    false
  | {data=ArrayDec(var_name,var_type,arr_expr)} ->
    let bt_of_vt = function
      | ArrayVT({data=ArrayAT(bt,_)},_,_) -> bt
      | _ -> raise CodegenError
    in
    let at_to_et = function
      | ArrayVT(at,lab,mut') -> ArrayET(at,lab,mut')
      | _ -> raise CodegenError
    in
    let bt_at_of_et = function
    | ArrayET({data=ArrayAT(bt,_)} as at,_,_) -> bt,at
    | _ -> raise CodegenError
    in
    let arr_expr, _ = arr_expr.data in
    let left_ty = at_to_et var_type.data in
    let alloca,add_to_type_env = codegen_array_expr cg_ctx var_name (arr_expr, left_ty) in
    (*let ct_verif_ty = bt_to_llvm_ty cg_ctx (bt_of_vt var_type.data).data in
    let ct_verif_ty' = pointer_type ct_verif_ty in
    let alloca' = build_bitcast alloca ct_verif_ty' "ddd" cg_ctx.builder in
    codegen_dec cg_ctx.verify_llvm var_type alloca' cg_ctx.llcontext cg_ctx.llmodule cg_ctx.builder;*)
    (*let zero = const_int (i32_type cg_ctx.llcontext) 0 in
    let ptr = build_gep alloca [| zero |] "arrptr" cg_ctx.builder in*)
    add_var cg_ctx.venv var_name alloca;
    add_var cg_ctx.vtenv var_name var_type;
    let bt,at = bt_at_of_et left_ty in
    if add_to_type_env then add_var cg_ctx.tenv var_name at;
    false
  | {data=BaseAssign(var_name,expr)} ->
    let v = find_var cg_ctx.venv var_name in
    let vt = find_var cg_ctx.vtenv var_name in
    let vt' = vt_to_llvm_ty cg_ctx vt.data in
    let expr' = codegen_ext cg_ctx vt' expr in
    build_store expr' v cg_ctx.builder |> ignore;
    false
  | {data=ArrayAssign(var_name,array_index,expr)} ->
    let v = find_var cg_ctx.venv var_name in
    let vt = find_var cg_ctx.vtenv var_name in
    let bt = match vt.data with
      | ArrayVT({data=ArrayAT(bt,_)},_,_) -> bt.data
      | _ -> raise CodegenError in
    let ll_ty = bt_to_llvm_ty cg_ctx bt in
    let index = codegen_expr cg_ctx array_index.data in
    let expr' = codegen_ext cg_ctx ll_ty expr in
    let zero = const_int (i32_type cg_ctx.llcontext) 0 in
    let some_arg =
      try Some(find_var cg_ctx.tenv var_name) with
        | _-> None in
    let indices,ptr = match some_arg with
      | None -> [| zero; index |],v
      | Some arg ->
        let indices = [| index |] in
        let ptr = build_load v "loadedassignptr" cg_ctx.builder in
        indices, ptr in
    let p = build_gep ptr indices "ptr" cg_ctx.builder in
    build_store expr' p cg_ctx.builder |> ignore;
    false
  | {data=Block(stms)} ->
    codegen_stms cg_ctx ret_ty stms
  | {data=If(cond,thenstms,elsestms)} ->

    let cond' = codegen_expr cg_ctx cond.data in
    let one = const_int (i1_type cg_ctx.llcontext) 1 in
    let cond_val = build_icmp Icmp.Eq cond' one "branchcompare" cg_ctx.builder in

    let start_bb = insertion_block cg_ctx.builder in
    let parent_function = block_parent start_bb in

    let then_bb = append_block cg_ctx.llcontext "thenbranch" parent_function in
    position_at_end then_bb cg_ctx.builder;
    let then_terminated = codegen_stms cg_ctx ret_ty thenstms in

    let new_then_bb = insertion_block cg_ctx.builder in


    let else_bb = append_block cg_ctx.llcontext "elsebranch" parent_function in
    position_at_end else_bb cg_ctx.builder;
    let else_terminated = codegen_stms cg_ctx ret_ty elsestms in

    let new_else_bb = insertion_block cg_ctx.builder in
    position_at_end start_bb cg_ctx.builder;
    build_cond_br cond_val then_bb else_bb cg_ctx.builder |> ignore;

    if not (then_terminated && else_terminated) then
        (let merge_bb = append_block cg_ctx.llcontext "branchmerge" parent_function in
        position_at_end merge_bb cg_ctx.builder;

        (* Only merge if the basic blocks did not terminate *)
        if not then_terminated then
        begin
          position_at_end new_then_bb cg_ctx.builder;
          build_br merge_bb cg_ctx.builder |> ignore;
          position_at_end merge_bb cg_ctx.builder
        end;
        if not else_terminated then
        begin
          position_at_end new_else_bb cg_ctx.builder;
          build_br merge_bb cg_ctx.builder |> ignore;
          position_at_end merge_bb cg_ctx.builder;
        end;
        false)
    else true
  | {data=For(var_name,base_type,low_expr,high_expr,statements)} ->
    let preheader = insertion_block cg_ctx.builder in
    let parent_function = block_parent preheader in
    let bb_check = append_block cg_ctx.llcontext "loop_check" parent_function in
    let bb_body = append_block cg_ctx.llcontext "loop_body" parent_function in
    let bb_end = append_block cg_ctx.llcontext "loop_end" parent_function in
    let i = find_var cg_ctx.venv var_name in
    let bt = bt_to_llvm_ty cg_ctx base_type.data in
    let low = codegen_ext cg_ctx bt low_expr in
    ignore(build_store low i cg_ctx.builder);
    ignore(build_br bb_check cg_ctx.builder);
    position_at_end bb_check cg_ctx.builder;
    let i' = build_load i var_name.data cg_ctx.builder in
    let high = codegen_ext cg_ctx bt high_expr in
    let cmp = if is_signed base_type.data then Icmp.Slt else Icmp.Ult in
    let cond = build_icmp cmp i' high "loopcond" cg_ctx.builder in
    ignore(build_cond_br cond bb_body bb_end cg_ctx.builder);
    position_at_end bb_body cg_ctx.builder;
    codegen_stms cg_ctx ret_ty statements |> ignore;
    let i'' = build_load i var_name.data cg_ctx.builder in
    let one = (const_int (bt_to_llvm_ty cg_ctx base_type.data) 1) in
    let incr = build_add i'' one "loopincr" cg_ctx.builder in
    ignore(build_store incr i cg_ctx.builder);
    ignore(build_br bb_check cg_ctx.builder);
    position_at_end bb_end cg_ctx.builder;
    false
  | {data=VoidFnCall(fun_name,arg_exprs)} ->
    (* TODO: refactor this with FnCall *)
    let fun_dec = get_fn cg_ctx.fenv fun_name in
    let callee = match lookup_function fun_name.data cg_ctx.llmodule with
      | Some fn -> fn
      | None -> raise CodegenError in
    let codegen_arg' = codegen_arg cg_ctx in
    let args' = List.map2 codegen_arg' arg_exprs fun_dec.args in
    build_call callee (Array.of_list args') "" cg_ctx.builder |> ignore;
    false
  | {data=VoidReturn} ->
    build_ret_void cg_ctx.builder |> ignore;
    true
  | {data=Return(expr)} ->
    begin
      match ret_ty with
        | Some BaseET(bt,label) ->
          let ty = (bt_to_llvm_ty cg_ctx bt.data) in
          let ret' = codegen_ext cg_ctx ty expr in
          ignore(build_ret ret' cg_ctx.builder)
        (* TODO: assert valid function here *)
        | Some ArrayET _ -> raise CodegenError (* TODO: Cannot retur an array yet *)
        | None -> raise CodegenError
    end;
    true

and codegen_stms cg_ctx ret_ty (stms : Tast.block) =
  let _,stms' = stms in
  let cg = codegen_stm cg_ctx ret_ty in
  List.fold_left (fun returned stm -> (cg stm) || returned) false stms'

let rec declare_prototypes llcontext llmodule builder fenv = function
  | ArrayGet(_,expr),_ ->
    declare_prototypes llcontext llmodule builder fenv expr.data
  | IntCast(_,expr),_ ->
    declare_prototypes llcontext llmodule builder fenv expr.data
  | BinOp(_,expr1,expr2),_ ->
    declare_prototypes llcontext llmodule builder fenv expr1.data;
    declare_prototypes llcontext llmodule builder fenv expr2.data
  | UnOp(_,expr),_ ->
    declare_prototypes llcontext llmodule builder fenv expr.data
  | TernOp(expr1,expr2,expr3),_ ->
    declare_prototypes llcontext llmodule builder fenv expr1.data;
    declare_prototypes llcontext llmodule builder fenv expr2.data;
    declare_prototypes llcontext llmodule builder fenv expr3.data
  | FnCall(fun_name,args_exprs),_ ->
    let {ret_ty; args} = Hashtbl.find fenv fun_name.data in
    declare_prototype llcontext llmodule builder fenv args ret_ty fun_name |> ignore;
    ()
  | _ -> ()

and declare_arg_prototypes cg_ctx llmodule builder fenv = function
    | ByValue expr ->
      declare_prototypes cg_ctx llmodule builder fenv expr.data
    | ByRef _ -> ()
    | ByArray _ -> raise CodegenError

and declare_prototype cg_ctx llmodule builder fenv params ret name =
  let param_types = List.map (param_to_type cg_ctx) params in
  (*let array_ty = array_type (i32_type cg_ctx.llcontext) 10 in*)
  let param_types' = Array.of_list param_types in
  let ret_ty = get_ret_ty cg_ctx ret in
  let ft = function_type ret_ty param_types' in
  let ft' =
    match lookup_function name.data llmodule with
      | None -> 
        declare_function name.data ft llmodule
      | Some f -> raise FunctionAlreadyDefined in
  let fentry = { ret_ty=ret; args=params } in
  Hashtbl.add fenv name.data fentry;
  ft'

let codegen_fun llcontext llmodule builder fenv verify_llvm = function
  | { data=FunDec(name,ret,params,body) } ->
    Log.info "Generating function, %s" name.data;
    let venv = Env.new_env () in
    let tenv = Env.new_env () in
    let vtenv = Env.new_env () in
    let cg_ctx = { llcontext; llmodule; builder; venv; fenv; tenv; vtenv; verify_llvm } in
    let ft = declare_prototype cg_ctx llmodule builder fenv params ret name in
    let bb = append_block llcontext "entry" ft in
    position_at_end bb builder;
    declare_ct_verif verify_llvm llcontext llmodule ASSUME;
    declare_ct_verif verify_llvm llcontext llmodule PUBLIC_IN;
    declare_ct_verif verify_llvm llcontext llmodule PUBLIC_OUT;
    declare_ct_verif verify_llvm llcontext llmodule DECLASSIFIED_OUT;
    declare_ct_verif verify_llvm llcontext llmodule SMACK_VALUE;
    declare_ct_verif verify_llvm llcontext llmodule SMACK_VALUES;
    declare_ct_verif verify_llvm llcontext llmodule SMACK_RETURN_VALUE;
    allocate_args cg_ctx params ft;
    allocate_stack cg_ctx body;
    let returned = 
      begin
        match ret with
          | None -> codegen_stms cg_ctx None body
          | Some ret' ->
            codegen_stms cg_ctx (Some ret'.data) body
      end in
    if not returned then build_ret_void builder |> ignore;

    (*let ret' = codegen_ext llcontext llmodule builder var_env ret in
    build_ret ret' builder;*)
    (*Llvm_analysis.assert_valid_function ft;*)
    ft
  | { data=CExtern(fun_name, ret_ty, params) } ->
    let venv = Env.new_env () in
    let tenv = Env.new_env () in
    let vtenv = Env.new_env () in
    let cg_ctx = { llcontext; llmodule; builder; venv; fenv; tenv; vtenv; verify_llvm } in
    declare_prototype cg_ctx llmodule builder fenv params ret_ty fun_name

let rec codegen_fdecs llcontext llmodule builder fenv verify = function
  | [] -> ()
  | fd::rest ->
    ignore(codegen_fun llcontext llmodule builder fenv verify fd);
    codegen_fdecs llcontext llmodule builder fenv verify rest

let rec codegen llcontext llmodule builder verify = function
  | Module(_,fdecs) ->
    Log.info "Codegening module";
    let fenv = new_fenv () in
      codegen_fdecs llcontext llmodule builder fenv verify fdecs
