open Tast
open Llvm
open Pos
open Ctverif
open Codegen_utils

exception CodegenError
exception FunctionAlreadyDefined

let counter = ref 0

let fake_pos = { file=""; line=0; lpos=0; rpos=0 }

(* Start env functionality. This is here because of circular dependencies. TODO: move it out *)

let has_fn = Hashtbl.mem

let get_fn fenv f =
  try
    Hashtbl.find fenv f.data
  with
    Not_found -> raise @@ Err.errFnNotDefined f

let add_fn = Hashtbl.add

let new_fenv oldfenv =
  let fenvs = Hashtbl.create 10 in
  let add n (fdec,_) =
    match fdec.data with
      | FunDec(_,_,ret_ty,args,_)
      | DebugFunDec(_,ret_ty,args)
      | StdlibFunDec(_,_,ret_ty,args)
      | CExtern(_,ret_ty,args) ->
        add_fn fenvs n {ret_ty; args}
  in
  Env.iter add oldfenv;
  fenvs

(* End env functionality *)


let mk_ctx llcontext llmodule builder venv fenv tenv vtenv verify_llvm =
  { llcontext; llmodule; builder; venv; fenv; tenv; vtenv; verify_llvm; sdecs=[] }

type intrinsic = 
  | Memcpy
  | Memset
  | Rotl of int
  | Rotr of int

let string_of_intrinsic = function
  | Memcpy -> "llvm.memcpy.p0i8.p0i8.i64"
  | Memset -> "llvm.memset.p0i8.i64"
  | Rotl n -> "__rotl" ^ (string_of_int n)
  | Rotr n -> "__rotr" ^ (string_of_int n)

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
  | Memset ->
    let i8_ty = i8_type cg_ctx.llcontext in
    let i32_ty = i32_type cg_ctx.llcontext in
    let i64_ty = i64_type cg_ctx.llcontext in
    let ptr_ty = pointer_type (i8_type cg_ctx.llcontext) in
    let bool_ty = i1_type cg_ctx.llcontext in
    let arg_types = [| ptr_ty; i8_ty; i64_ty; i32_ty; bool_ty |] in
    let vt = void_type cg_ctx.llcontext in
    let ft = function_type vt arg_types in
      declare_function (string_of_intrinsic Memset) ft cg_ctx.llmodule
  | Rotl n as rotl_sz ->
    (* we expect this function to get inlined and disappear at high optimization levels *)
    let ity = integer_type cg_ctx.llcontext n in
    let ft = function_type ity [| ity; ity |] in
    let fn = declare_function (string_of_intrinsic rotl_sz) ft cg_ctx.llmodule in
      add_function_attr fn Alwaysinline;
      set_linkage Internal fn;
    let bb = append_block cg_ctx.llcontext "entry" fn in
    let b = builder cg_ctx.llcontext in
      position_at_end bb b;
      let e1 = param fn 0 in
      let e2 = param fn 1 in
        set_value_name "_secret_x" e1;
        set_value_name "_secret_n" e2;
      let lshift = build_shl e1 e2 "_secret_lshift" b in
      let subtmp = build_sub (const_int (type_of e1) n) e2 "_secret_subtmp" b in
      let lrshift = build_lshr e1 subtmp "_secret_lrshift" b in
      let rotltmp = build_or lshift lrshift "_secret_rotltmp" b in
        build_ret rotltmp b;
        fn
  | Rotr n as rotr_sz ->
    (* we expect this function to get inlined and disappear at high optimization levels *)
    let ity = integer_type cg_ctx.llcontext n in
    let ft = function_type ity [| ity; ity |] in
    let fn = declare_function (string_of_intrinsic rotr_sz) ft cg_ctx.llmodule in
      add_function_attr fn Alwaysinline;
      set_linkage Internal fn;
    let bb = append_block cg_ctx.llcontext "entry" fn in
    let b = builder cg_ctx.llcontext in
      position_at_end bb b;
      let e1 = param fn 0 in
      let e2 = param fn 1 in
        set_value_name "_secret_x" e1;
        set_value_name "_secret_n" e2;
      let lrshift = build_lshr e1 e2 "_secret_lrshift" b in
      let subtmp = build_sub (const_int (type_of e1) n) e2 "_secret_subtmp" b in
      let lshift = build_shl e1 subtmp "_secret_lshift" b in
      let rotrtmp = build_or lrshift lshift "_secret_rotrtmp" b in
        build_ret rotrtmp b;
        fn

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

let is_dynamic_sized_array = function
  | ArrayVT({data=ArrayAT(_,{data=LDynamic _})},_,_) -> true
  | _ -> false

let vt_to_llvm_ty (cg_ctx : codegen_ctx_record) = function
  | RefVT({data=base_type},maybe_label,_) ->
    bt_to_llvm_ty cg_ctx.llcontext base_type
  | ArrayVT({data=ArrayAT(at,size)},maybe_label,_) ->
    let arr_ty = bt_to_llvm_ty cg_ctx.llcontext at.data in
    let size = get_size cg_ctx size.data in
    (* TODO: Should we be passing arrays as a * or **? *)
    array_type arr_ty size

let param_to_type cg_ctx = function
  | {data=Param(var_name,
    {data=RefVT({data=base_type},maybe_label,{data=Mut})})} ->
    pointer_type(bt_to_llvm_ty cg_ctx.llcontext base_type)
  | {data=Param(var_name,
    {data=RefVT({data=base_type},maybe_label,{data=Const})})} ->
    bt_to_llvm_ty cg_ctx.llcontext base_type
  | {data=Param(var_name,{data=ArrayVT({data=ArrayAT(bt,size)} as ty,maybe_label,_)})} ->
    begin
      match size.data with
        | LIntLiteral s ->
          pointer_type(array_type (bt_to_llvm_ty cg_ctx.llcontext bt.data) s)
        | LDynamic _ ->
          Hashtbl.add (Env.get_vtbl cg_ctx.tenv) var_name.data ty;
          pointer_type (bt_to_llvm_ty cg_ctx.llcontext bt.data)
    end
  | {data=Param(var_name,{data=StructVT(s,_)})} ->
    let struct_ty = List.assoc s.data cg_ctx.sdecs in
      pointer_type struct_ty

let bitsize cg_ctx = function
  | BaseET({data=base_type},_) ->
    bt_to_llvm_ty cg_ctx.llcontext base_type
  | ArrayET({data=ArrayAT(at,size)},_,_) ->
    bt_to_llvm_ty cg_ctx.llcontext at.data

let expr_ty_to_llvm_ty cg_ctx = function
  | BaseET({data=base_type},_) ->
    bt_to_llvm_ty cg_ctx.llcontext base_type
  | ArrayET({data=ArrayAT(at,size)},_,_) ->
    let arr_ty = bt_to_llvm_ty cg_ctx.llcontext at.data in
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


let make_name str ml =
  let make_prefix = function
    | Public  -> "_public_"
    | Secret  -> "_secret_"
    | Unknown -> "_unknown_" in
  match ml with
  | {data=Fixed l} -> (make_prefix l) ^ str
  | {data=Guess(s,l)} -> (make_prefix !l) ^ "guess_" ^ str

let make_name_et str = function
  | BaseET(_, ml) -> make_name str ml
  | ArrayET(_,ml,_) -> make_name str ml

let make_name_vt str = function
  | RefVT(_,ml,_) -> make_name str ml
  | ArrayVT(_,ml,_) -> make_name str ml

let make_name_ll str llvalue =
  let name = value_name llvalue in
  let prefix = ref None in
  let set_prefix p =
    match Core.String.is_prefix name ~prefix:p with
      | true -> prefix := Some p
      | false -> () in
  set_prefix "_secret_";
  set_prefix "_public_";
  set_prefix "_unknown_";
  match !prefix with
    | None -> raise CodegenError
    | Some p -> p ^ str

(* Allocate all of the args for a function *)
let allocate_args cg_ctx args f =
  let allocate_arg ({data=Param(var_name,var_type)} as arg) ll_arg =
    let () =
    match var_type.data with
      | RefVT (bt,ml,{data=Mut}) ->
        let name = make_name var_name.data ml in
        codegen_dec cg_ctx var_type ll_arg;
        Env.add_var cg_ctx.venv var_name ll_arg;
        set_value_name name ll_arg
      | RefVT (bt,ml,{data=Const}) ->
        let ty = param_to_type cg_ctx arg in
        let name = make_name var_name.data ml in
        let alloca = build_alloca ty name cg_ctx.builder in
        codegen_dec cg_ctx var_type ll_arg;
        Env.add_var cg_ctx.venv var_name alloca;
        set_value_name name ll_arg;
        build_store ll_arg alloca cg_ctx.builder |> ignore
      (*| ArrayVT({data=ArrayAT(bt,{data=LIntLiteral(s)})},_,_) ->
        let ty = array_type (bt_to_llvm_ty cg_ctx bt.data) s in
        let size = const_int (i32_type cg_ctx.llcontext) s in
        let loaded = build_load ll_arg "loadedarrptr" cg_ctx.builder in
        let alloca = build_alloca ty var_name.data cg_ctx.builder in
        add_var cg_ctx.venv var_name ll_arg;
        ()*)
      (* We cannot pass a static array to a function.
         Instead, it must be a dyn array *)
      | ArrayVT({data=ArrayAT(bt,{data=_(*LDynamic(var_name')*)})},ml,_) ->
        let ty = pointer_type(bt_to_llvm_ty cg_ctx.llcontext bt.data) in
        let name = make_name "arrarg" ml in
        let alloca = build_alloca ty name cg_ctx.builder in
        codegen_dec cg_ctx var_type ll_arg;
        Env.add_var cg_ctx.venv var_name alloca;
        set_value_name name ll_arg;
        build_store ll_arg alloca cg_ctx.builder |> ignore
      | StructVT(s,_) ->
        let struct_ty = List.assoc s.data cg_ctx.sdecs in
        let ty = pointer_type struct_ty in
        let name = make_name "structarg" (make_ast fake_pos (Fixed Public)) in
        let alloca = build_alloca ty name cg_ctx.builder in
          (* XXX codegen_dec *)
          Env.add_var cg_ctx.venv var_name alloca;
          set_value_name name ll_arg;
          build_store ll_arg alloca cg_ctx.builder |> ignore
    in

    (*let llvm_ty = param_to_type cg_ctx arg in
    let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in*)
    Env.add_var cg_ctx.vtenv var_name var_type
  in
  let ll_args = Array.to_list (params f) in
  List.iter2 allocate_arg args ll_args

(* Allocate space for each variable declared inside a function. This is
   done at the beginning of the function. *)
(* NOTE(src): we really should find a way to get this directly from the tast venvs
   instead of descending through the AST again *)
let rec allocate_stack cg_ctx stms =
  let rec allocate_inject {data=(expr,_)} =
    match expr with
      | Lvalue lv -> allocate_lval lv
      | IntCast(_,e) -> allocate_inject e
      | UnOp(_,e) -> allocate_inject e
      | BinOp(_,e1,e2) -> allocate_inject e1; allocate_inject e2
      | TernOp(e1,e2,e3) -> allocate_inject e1; allocate_inject e2; allocate_inject e3
      | Select(e1,e2,e3) -> allocate_inject e1; allocate_inject e2; allocate_inject e3
      | FnCall(f,args) -> () (* XXX descend into the exprs in args *)
      | Declassify e -> allocate_inject e
      | Inject(_,stms) -> List.iter allocate_stack' stms
      | _ -> ()
  and allocate_lval {data=(lval,_)} =
    match lval with
      | Base _ -> ()
      | ArrayEl(lv,n) -> allocate_lval lv; allocate_inject n
      | StructEl(lv,_) -> allocate_lval lv
  and allocate_stack' = function
    | {data=BaseDec(var_name,var_type,expr)} ->
      allocate_inject expr;
      let llvm_ty = vt_to_llvm_ty cg_ctx var_type.data in
      let name = make_name_vt var_name.data var_type.data in
      let alloca = build_alloca llvm_ty name cg_ctx.builder in
      Env.add_var cg_ctx.venv var_name alloca;
      Env.add_var cg_ctx.vtenv var_name var_type
    | {data=ArrayDec(var_name,var_type,expr)} -> ()
      (*let llvm_ty = vt_to_llvm_ty cg_ctx.llcontext var_type.data in*)
      (* TODO: I think this will fail. I think this will just allocate a
         pointer. But we want it to allocate the space for the array.
         So yea, fix dis *)
      (*let alloca = build_alloca llvm_ty var_name.data cg_ctx.builder in
      add_var cg_ctx.venv var_name alloca*)
    | {data=Assign(lv,expr)} -> allocate_lval lv; allocate_inject expr
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
      let ml = make_ast fake_pos (Fixed Public) in
      let name = make_name var_name.data ml in
      let alloca = build_alloca llvm_ty name cg_ctx.builder in
      (* TODO: Fix the scoping here. This will force us to use a different
         var name for the loop iterator for each for loop per function. We
         probably want to be able to reuse this?? *)
      Env.add_var cg_ctx.venv var_name alloca;
      allocate_stack cg_ctx stms
    | {data=Return(expr)} -> allocate_inject expr
    | {data=VoidFnCall _} -> ()
    | {data=DebugVoidFnCall _} -> ()
    | {data=VoidReturn} -> ()
  in
  let env,stms' = stms in
  ignore(List.map allocate_stack' stms')

let codegen_binop cg_ctx op e1 e2 ty ety ml b =
  let e1_width = integer_bitwidth (type_of e1) in
  let e2_width = integer_bitwidth (type_of e2) in
  let e1,e2 =
    match e1_width,e2_width with
      | w1,w2 when w1 < w2 ->
        let name = make_name "lhssext" ml in
        let e1' = (build_sext e1 (type_of e2)) name cg_ctx.builder in
        e1',e2
      | w1,w2 when w1 > w2 -> 
        let name = make_name "rhssext" ml in
        let e2' = (build_sext e2 (type_of e1)) name cg_ctx.builder in
        e1,e2'
      | w1,w2 -> e1,e2 in
  let res = 
    match op with
      | Ast.Plus -> build_add e1 e2 (make_name "addtmp" ml) b
      | Ast.Minus -> build_sub e1 e2 (make_name "subtmp" ml) b
      | Ast.Multiply -> build_mul e1 e2 (make_name "multmp" ml) b
      | Ast.Divide when is_signed ty -> build_sdiv e1 e2 (make_name "sdivtmp" ml) b
      | Ast.Modulo when is_signed ty -> build_srem e1 e2 (make_name "sremtmp" ml) b
      | Ast.Divide -> build_udiv e1 e2 (make_name "udivtmp" ml) b
      | Ast.Modulo -> build_urem e1 e2 (make_name "uremtmp" ml) b
      | Ast.Equal -> build_icmp Icmp.Eq e1 e2 (make_name "eqtmp" ml) b
      | Ast.NEqual -> build_icmp Icmp.Ne e1 e2 (make_name "neqtmp" ml) b
      | Ast.GT when is_signed ety -> build_icmp Icmp.Sgt e1 e2 (make_name "sgttmp" ml) b
      | Ast.GTE when is_signed ety -> build_icmp Icmp.Sge e1 e2 (make_name "sgtetmp" ml) b
      | Ast.LT when is_signed ety -> build_icmp Icmp.Slt e1 e2 (make_name "slttmp" ml) b
      | Ast.LTE when is_signed ety -> build_icmp Icmp.Sle e1 e2 (make_name "sltetmp" ml) b
      | Ast.GT -> build_icmp Icmp.Ugt e1 e2 (make_name "ugttmp" ml) b
      | Ast.GTE -> build_icmp Icmp.Uge e1 e2 (make_name "ugtetmp" ml) b
      | Ast.LT -> build_icmp Icmp.Ult e1 e2 (make_name "ulttmp" ml) b
      | Ast.LTE -> build_icmp Icmp.Ule e1 e2 (make_name "ultetmp" ml) b
      | Ast.LogicalAnd -> build_and e1 e2 (make_name "landtmp" ml) b
      | Ast.LogicalOr -> build_or e1 e2 (make_name "lortmp" ml) b
      | Ast.BitwiseAnd -> build_and e1 e2 (make_name "andtmp" ml) b
      | Ast.BitwiseOr -> build_or e1 e2 (make_name  "ortmp" ml) b
      | Ast.BitwiseXor -> build_xor e1 e2 (make_name "xortmp" ml) b
      | Ast.LeftShift -> build_shl e1 e2 (make_name "lshift" ml) b
      | Ast.RightShift when is_signed ty ->
        build_ashr e1 e2 (make_name "arshift" ml) b
      | Ast.RightShift -> build_lshr e1 e2 (make_name "lrshift" ml) b
      | Ast.LeftRotate ->
        (* counting on the optimizer to optimize this into a single instruction *)
        let UInt n = ty in
        let rotl_fn = get_intrinsic (Rotl n) cg_ctx in
          build_call rotl_fn [| e1; e2 |] "rotltmp" b
      | Ast.RightRotate ->
        (* counting on the optimizer to optimize this into a single instruction *)
        let UInt n = ty in
        let rotr_fn = get_intrinsic (Rotr n) cg_ctx in
          build_call rotr_fn [| e1; e2 |] "rotrtmp" b
  in

  let ret_ty = bt_to_llvm_ty cg_ctx.llcontext ty in
  let ret_width = integer_bitwidth ret_ty in
  let expr_width = integer_bitwidth (type_of res) in
  match ret_width,expr_width with
    | rw,ew when rw < ew ->
      build_trunc res ret_ty (make_name "truncbinop" ml) cg_ctx.builder
    | rw,ew when rw > ew ->
      build_sext res ret_ty (make_name "sextbinop" ml) cg_ctx.builder
    | rw,ew -> res

let codegen_unop builder value ml = function
  | Ast.Neg -> build_neg value (make_name "negtmp" ml) builder
  | Ast.LogicalNot -> build_not value (make_name "lnottmp" ml) builder
  | Ast.BitwiseNot -> build_not value (make_name "bnottmp" ml) builder

let build_cast ty cg_ctx value = (* from, to *) function
  | Bool ->
    let name = make_name_et "bcast" ty in
    build_intcast value (bt_to_llvm_ty cg_ctx.llcontext Bool) name cg_ctx.builder
  | UInt(n) ->
    let name = make_name_et "ucast" ty in
    let m = integer_bitwidth (type_of value) in
      (if n > m then build_zext else build_trunc)
        value (bt_to_llvm_ty cg_ctx.llcontext (UInt n)) name cg_ctx.builder
  | Int(n) ->
    let name = make_name_et "icast" ty in
    let m = integer_bitwidth (type_of value) in
      (if n > m then build_sext else build_trunc)
        value (bt_to_llvm_ty cg_ctx.llcontext (Int n)) name cg_ctx.builder
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
  (*let is_dynamic_array = function
    | ArrayView _,_ -> true
    | ArrayVar lval,_ ->
      let some_arg =
        try Some(Env.find_var cg_ctx.tenv var_name) with
          | _ -> None in
      begin
      match some_arg with
        | None -> false
        | Some arg -> true
      end
    | _,ArrayET({data=ArrayAT(_,{data=LDynamic _})},_,_) -> true
    | _,ArrayET({data=ArrayAT(_,{data=LIntLiteral _})},_,_) -> false
    | _ -> raise CodegenError in*)
  let is_dynamic_array arr =
    let pty = type_of arr in
    let base = element_type pty in
    let kind = classify_type base in
      match kind with
        | TypeKind.Pointer -> true
        | _ -> false in
  let vt =
    match ty.data with
      | Param(_,vt) -> vt.data in
  match arg.data with
    | ByValue expr ->
      codegen_ext cg_ctx (vt_to_llvm_ty cg_ctx vt) expr
    | ByArray(arr,_) ->
      begin
        match ty.data with
          | Param(name,{data=ArrayVT({data=ArrayAT(bt,lexpr)},ml,_)}) ->
            let arr',_ = codegen_array_expr cg_ctx name arr.data in
            let arr =
            begin
              match lexpr.data, is_dynamic_array arr' with
                | LIntLiteral s,false ->
                  let arr',_ = codegen_array_expr cg_ctx name arr.data in
                  build_load arr' (make_name "arr" ml) cg_ctx.builder |> ignore;
                  arr'
                | LIntLiteral s,true ->
                  let arr',_ = codegen_array_expr cg_ctx name arr.data in
                  let arr_type = array_type (bt_to_llvm_ty cg_ctx.llcontext bt.data) s in
                  let pt = pointer_type arr_type in
                  let name = make_name "dyntostaticarr" ml in
                  build_bitcast arr' pt name cg_ctx.builder
                | LDynamic var_name,true ->
                  let arr',_ = codegen_array_expr cg_ctx name arr.data in
                  let name = make_name "loadeddynarrarg" ml in
                  build_load arr' name cg_ctx.builder
                | LDynamic var_name,false ->
                  let arr',_ = codegen_array_expr cg_ctx name arr.data in
                  let ll_ty = bt_to_llvm_ty cg_ctx.llcontext bt.data in
                  let name = make_name "arrtoptr" ml in
                    build_bitcast arr' (pointer_type ll_ty) name cg_ctx.builder
            end in
            (*remove_var cg_ctx.tenv name;*)
            arr
          | _-> raise CodegenError
      end;
    | ByRef r ->
      let var = codegen_lval cg_ctx r in
      match vt with
        | RefVT(_,ml,{data=Const})
        | ArrayVT(_,ml,{data=Const}) ->
          build_load var (make_name "argref" ml) cg_ctx.builder
        | RefVT(_,_,{data=Mut})
        | ArrayVT(_,_,{data=Mut}) -> var

and codegen_lval cg_ctx {data=(lval,vt);pos=p} =
  match lval with
    | Base var_name ->
      Env.find_var cg_ctx.venv var_name
    | ArrayEl(lv,expr) ->
      let arr' = codegen_lval cg_ctx lv in
      let index = codegen_expr cg_ctx expr.data in
      let zero = const_int (type_of index) 0 in
      let indices, arr =
        begin
          match arr' |> type_of |> element_type |> classify_type with
            | TypeKind.Pointer ->
              [| index |], build_load arr' (make_name_vt "dynload" vt) cg_ctx.builder
            | _ -> [| zero; index |], arr'
        end in
        build_in_bounds_gep arr indices (make_name_vt "ptr" vt) cg_ctx.builder
    | StructEl _ ->
      raise CodegenError

and codegen_expr cg_ctx = function
  | True, ty -> const_all_ones (expr_ty_to_llvm_ty cg_ctx ty)
  | False, ty -> const_null (expr_ty_to_llvm_ty cg_ctx ty)
  | IntLiteral i, ty -> const_int (expr_ty_to_llvm_ty cg_ctx ty) i
  | StringLiteral s, ty ->
    build_global_stringptr s (make_name_et "str" ty) cg_ctx.builder
  | Lvalue lval, ty ->
    let cell = codegen_lval cg_ctx lval in
      build_load cell (make_name_et "lval" ty) cg_ctx.builder
  | IntCast(base_ty,expr), ty ->
    let v = codegen_expr cg_ctx expr.data in
    build_cast ty cg_ctx v base_ty.data
  | UnOp(op,expr), ty ->
    let llty = expr_ty_to_llvm_ty cg_ctx ty in
    let expr' = codegen_ext cg_ctx llty expr in
    let ml = match ty with
      | BaseET(bt,ml) -> ml
      | ArrayET _ -> raise CodegenError in
    codegen_unop cg_ctx.builder expr' ml op
  | BinOp(op,expr1,expr2), ty ->
    let ty',ml = match ty with
      | BaseET(bt, ml) -> bt, ml
      | ArrayET _ -> raise CodegenError in
    let b1 = Tast_utils.(expr_to_btype expr1) in
    let b2 = Tast_utils.(expr_to_btype expr2) in
      (* assigning ty' to ety is just a placeholder; it doesn't actually get used *)
      (* I [scauligi] should probably be using an option type instead but I'm lazy and this is easier *)
    let ety = Tast_utils.(if joinable_bt b1 b2 then join_bt expr1.pos b1 b2 else ty') in
    let e1 = codegen_expr cg_ctx expr1.data in
    let e2 = codegen_expr cg_ctx expr2.data in
    codegen_binop cg_ctx op e1 e2 ty'.data ety.data ml cg_ctx.builder
  | TernOp(expr1,expr2,expr3), ty ->
    let e1 = codegen_expr cg_ctx expr1.data in
    let e1' = build_is_not_null e1 (make_name_et "condtmp" ty) cg_ctx.builder in
    let ty' = expr_ty_to_llvm_ty cg_ctx ty in
    let e2 = codegen_ext cg_ctx ty' expr2 in
    let e3 = codegen_ext cg_ctx ty' expr3 in
    build_select e1' e2 e3 (make_name_et "terntmp" ty) cg_ctx.builder
  | FnCall(fun_name,args), ty ->
    let fun_dec = get_fn cg_ctx.fenv fun_name in
    let callee = match lookup_function fun_name.data cg_ctx.llmodule with
      | Some fn -> fn
      | None -> Stdlib.get_stdlib fun_name.data cg_ctx.llcontext cg_ctx.llmodule in
    let codegen_arg' = codegen_arg cg_ctx in
    let args' = List.map2 codegen_arg' args fun_dec.args in
    let name = make_name_et "calltmp" ty in
    build_call callee (Array.of_list args') name cg_ctx.builder
  | DebugFnCall(_,_),_ -> raise CodegenError
  | Declassify(expr), ty ->
    let e = codegen_expr cg_ctx expr.data in
    let n = value_name e in
      set_value_name ("_declassified_" ^ n) e; e
  | Select(expr1,expr2,expr3), ty ->
    let e1 = codegen_expr cg_ctx expr1.data in
    let e1' = build_is_not_null e1 (make_name_et "condtmp" ty) cg_ctx.builder in
    let ty' = expr_ty_to_llvm_ty cg_ctx ty in
    let e2 = codegen_ext cg_ctx ty' expr2 in
    let e3 = codegen_ext cg_ctx ty' expr3 in
    build_select e1' e2 e3 (make_name_et "selecttmp" ty) cg_ctx.builder
  | Inject(var_name,stms), ty ->
    let ret_ty = None in
    ignore(List.map (codegen_stm cg_ctx ret_ty) stms);
    let store = Env.find_var cg_ctx.venv var_name in
    build_load store (make_name_et var_name.data ty) cg_ctx.builder

and extend_to ctx builder signed dest et v =
  let llvm_et = expr_ty_to_llvm_ty ctx et in
  let lb,rb = integer_bitwidth dest, integer_bitwidth llvm_et in
  match lb,rb with
    | lb',rb' when lb' = rb' -> v
    | lb',rb' when lb' < rb' ->
      build_trunc v dest (make_name_et "trunctmp" et) ctx.builder
    | lb',rb' when (lb' > rb') && signed ->
      build_sext v dest (make_name_et "sexttmp" et) ctx.builder
    | lb',rb' when lb' > rb' ->
      build_zext v dest (make_name_et "zexttmp" et) ctx.builder
    | _ -> raise CodegenError (* This should never be hit *)

and codegen_ext cg_ctx dest (expr : expr) =
  match expr.data with
    | expr',ty' ->
      let expr' = codegen_expr cg_ctx expr.data in
      begin
        match classify_type dest with
          | TypeKind.Pointer -> expr'
          | _ -> extend_to cg_ctx cg_ctx.builder true dest ty' expr'
      end

and vt_to_bt = function
  | RefVT(bt,_,_) -> bt.data
  | ArrayVT _ -> raise CodegenError

and codegen_array_expr cg_ctx arr_name = function
  (* XXX gary here too pls *)
  | ArrayVar lval,ty -> codegen_lval cg_ctx lval,false
  | ArrayLit exprs,ty ->
    (* TODO: This needs optimization. We want this array to be global if
             all exprs are known at compile time. Side note -- this is
             what clang does.*)
    let bitsize = bitsize cg_ctx ty in
    let ll_exprs' = List.map (codegen_ext cg_ctx bitsize) exprs in
    let arr_ty = array_type bitsize (List.length ll_exprs') in
    let zero = const_int bitsize 0 in
    let alloca =
      build_alloca arr_ty (make_name_et "arraylit" ty) cg_ctx.builder in
    let gep i el =
      let i' = const_int (i32_type cg_ctx.llcontext) i in
      let name = make_name_et "index" ty in
      let ptr = build_in_bounds_gep alloca [| zero; i' |] name cg_ctx.builder in
      build_store el ptr cg_ctx.builder |> ignore
      in
    List.iteri gep ll_exprs';
    alloca,false
  | ArrayZeros lexpr,ty ->
    begin
      match lexpr.data with
        | LIntLiteral n ->
          let ty' = expr_ty_to_base_ty ty in
          let ll_ty = bt_to_llvm_ty cg_ctx.llcontext ty' in
          let arr_ty = array_type ll_ty n in
          let name = make_name_et "zerodarray" ty in
          let alloca = build_alloca arr_ty name cg_ctx.builder in
          let pointer_ty = pointer_type (i8_type cg_ctx.llcontext) in
          let name = make_name_et "sourcecasted" ty in
          let source_casted =
            build_bitcast alloca pointer_ty name cg_ctx.builder in
          let zero = const_int (i8_type cg_ctx.llcontext) 0 in
          let sz = const_int (i64_type cg_ctx.llcontext) (n * (byte_size_of_expr_ty ty)) in
          let alignment = (const_int (i32_type cg_ctx.llcontext) 0) in
          let volatility = (const_int (i1_type cg_ctx.llcontext) 0) in
          let args = [| source_casted; zero; sz; alignment; volatility |] in
          let memset = get_intrinsic Memset cg_ctx in
            build_call memset args "" cg_ctx.builder;
            alloca,false
        | LDynamic x -> raise CodegenError
    end
  | ArrayCopy lval,ty ->
    let ll_ty = expr_ty_to_llvm_ty cg_ctx ty in
    let name = make_name_et "copiedarray" ty in
    let alloca = build_alloca ll_ty name cg_ctx.builder in
    let from = codegen_lval cg_ctx lval in
    let cpy_len = array_length ll_ty in
    let num_bytes = (byte_size_of_expr_ty ty) * cpy_len in
    let ll_cpy_len = (const_int (i64_type cg_ctx.llcontext) num_bytes) in
    let alignment = (const_int (i32_type cg_ctx.llcontext) 0) in
    let volatility = (const_int (i1_type cg_ctx.llcontext) 0) in
    let source_cast_ty = pointer_type (i8_type cg_ctx.llcontext) in
    let source_val, source_ty =
      begin
        match from |> type_of |> element_type |> classify_type with
          | TypeKind.Pointer ->
            let name = make_name_et "loadedtocopy" ty in
            let source_casted = build_load from name cg_ctx.builder in
            let name = make_name_et "sourcecasted" ty in
            let source_casted' =
              build_bitcast source_casted source_cast_ty name cg_ctx.builder in
              source_casted', source_cast_ty
          | _ ->
            let name = make_name_et "sourcecasted" ty in
            let source_casted =
              build_bitcast from source_cast_ty name cg_ctx.builder in
              source_casted, source_cast_ty
      end in
    let name = make_name_et "destcast" ty in
    let dest_casted = build_bitcast alloca source_ty name cg_ctx.builder in
    let args = [| dest_casted; source_val; ll_cpy_len; alignment; volatility |] in
    let memcpy = get_intrinsic Memcpy cg_ctx in
    build_call memcpy args "" cg_ctx.builder |> ignore;
    alloca,false

  | ArrayView(lval, expr, lexpr),ty ->
    let index = codegen_expr cg_ctx expr.data in
    let from = codegen_lval cg_ctx lval in
    let bt_at_of_et = function
      | ArrayET({data=ArrayAT(bt,_)} as at,_,_) -> bt,at
      | _ -> raise CodegenError
    in
    let r =
      begin
        match from |> type_of |> element_type |> classify_type with
          | TypeKind.Pointer ->
            let indices = [| index |] in
            let name = make_name_et "loadedviewptr" ty in
            let ptr = build_load from name cg_ctx.builder in
            let name = make_name_et "source_gep" ty in
            let source_gep = build_in_bounds_gep ptr indices name cg_ctx.builder in
            let bt,at = bt_at_of_et ty in
            let ty' = pointer_type (bt_to_llvm_ty cg_ctx.llcontext bt.data) in
            let name = make_name_et "arrviewdyn" ty in
            let alloca = build_alloca ty' name cg_ctx.builder in
              build_store source_gep alloca cg_ctx.builder |> ignore;
              alloca,true
          | _ ->
            let bt,at = bt_at_of_et ty in
            let ty' = pointer_type (bt_to_llvm_ty cg_ctx.llcontext bt.data) in
            let name = make_name_et "arrview" ty in
            let alloca = build_alloca ty' name cg_ctx.builder in
            let zero = const_int (type_of index) 0 in
            let indices = [| zero; index |] in
            let name = make_name_et "source_gep" ty in
            let source_gep = build_in_bounds_gep from indices name cg_ctx.builder in
              build_store source_gep alloca cg_ctx.builder |> ignore;
              alloca,true
      end in
      r
  | ArrayComp(bt,lexpr, var_name, expr),ty -> raise CodegenError
  | ArrayNoinit lexpr,ty ->
    begin
      match lexpr.data with
        | LIntLiteral n ->
          let ty' = expr_ty_to_base_ty ty in
          let ll_ty = bt_to_llvm_ty cg_ctx.llcontext ty' in
          let arr_ty = array_type ll_ty n in
          let name = make_name_et "noinitarray" ty in
          let alloca = build_alloca arr_ty name cg_ctx.builder in
            alloca,false
        | LDynamic x -> raise CodegenError
    end

and codegen_stm cg_ctx ret_ty = function
  | {data=BaseDec(var_name,var_type,expr)} ->
    let v = Env.find_var cg_ctx.venv var_name in
    let expr' = codegen_ext cg_ctx (vt_to_llvm_ty cg_ctx var_type.data) expr in
    let s = build_store expr' v cg_ctx.builder in
    (*codegen_dec cg_ctx.verify_llvm var_type expr' cg_ctx.llcontext cg_ctx.llmodule cg_ctx.builder;*)
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
    let ct_verif_ty = bt_to_llvm_ty cg_ctx.llcontext (bt_of_vt var_type.data).data in
    let ct_verif_ty' = pointer_type ct_verif_ty in
    (*let alloca' = build_bitcast alloca ct_verif_ty' "ddd" cg_ctx.builder in
    codegen_dec cg_ctx.verify_llvm var_type alloca' cg_ctx.llcontext cg_ctx.llmodule cg_ctx.builder;*)
    (*let zero = const_int (i32_type cg_ctx.llcontext) 0 in
    let ptr = build_gep alloca [| zero |] "arrptr" cg_ctx.builder in*)
    Env.add_var cg_ctx.venv var_name alloca;
    Env.add_var cg_ctx.vtenv var_name var_type;
    let bt,at = bt_at_of_et left_ty in
    if add_to_type_env then Env.add_var cg_ctx.tenv var_name at;
    false
  | {data=Assign(lval,expr)} ->
    let v = codegen_lval cg_ctx lval in
    let (_,vt) = lval.data in
    let vt' = vt_to_llvm_ty cg_ctx vt in
    let expr' = codegen_ext cg_ctx vt' expr in
      build_store expr' v cg_ctx.builder;
      false
  | {data=Block(stms)} ->
    codegen_stms cg_ctx ret_ty stms
  | {data=If(cond,thenstms,elsestms)} ->

    let cond' = codegen_expr cg_ctx cond.data in
    let one = const_int (i1_type cg_ctx.llcontext) 1 in
    let _,cond_ty = cond.data in
    let name = make_name_et "branchcompare" cond_ty in
    let cond_val = build_icmp Icmp.Eq cond' one name cg_ctx.builder in

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
    let i = Env.find_var cg_ctx.venv var_name in
    let ml = make_ast fake_pos (Fixed Public) in
    let name_i = make_name var_name.data ml in
    set_value_name name_i i;
    let bt = bt_to_llvm_ty cg_ctx.llcontext base_type.data in
    let low = codegen_ext cg_ctx bt low_expr in
    ignore(build_store low i cg_ctx.builder);
    ignore(build_br bb_check cg_ctx.builder);
    position_at_end bb_check cg_ctx.builder;
    let ml = make_ast fake_pos (Fixed Public) in
    let name = make_name var_name.data ml in
    let i' = build_load i name cg_ctx.builder in
    let high = codegen_ext cg_ctx bt high_expr in
    let cmp = if is_signed base_type.data then Icmp.Slt else Icmp.Ult in
    let name = make_name "loopcond" ml in
    let cond = build_icmp cmp i' high name cg_ctx.builder in
    ignore(build_cond_br cond bb_body bb_end cg_ctx.builder);
    position_at_end bb_body cg_ctx.builder;
    codegen_stms cg_ctx ret_ty statements |> ignore;
    let i'' = build_load i name_i cg_ctx.builder in
    let one = (const_int (bt_to_llvm_ty cg_ctx.llcontext base_type.data) 1) in
    let name = make_name "loopincr" ml in
    let incr = build_add i'' one name cg_ctx.builder in
    ignore(build_store incr i cg_ctx.builder);
    ignore(build_br bb_check cg_ctx.builder);
    position_at_end bb_end cg_ctx.builder;
    false
  | {data=VoidFnCall(fun_name,arg_exprs)} ->
    (* TODO: refactor this with FnCall *)
    let fun_dec = get_fn cg_ctx.fenv fun_name in
    let callee = match lookup_function fun_name.data cg_ctx.llmodule with
      | Some fn -> fn
      | None -> Stdlib.get_stdlib fun_name.data cg_ctx.llcontext cg_ctx.llmodule in
    let codegen_arg' = codegen_arg cg_ctx in
    let args' = List.map2 codegen_arg' arg_exprs fun_dec.args in
    build_call callee (Array.of_list args') "" cg_ctx.builder |> ignore;
    false
  | {data=DebugVoidFnCall(fun_name,arg_exprs)} ->
    let f = 
    match lookup_function fun_name.data cg_ctx.llmodule with
      | None -> Debugfun.codegen_proto cg_ctx.llcontext cg_ctx.llmodule fun_name
      | Some f -> f in
    let fun_dec = get_fn cg_ctx.fenv fun_name in
    let args = List.map2 (codegen_arg cg_ctx) arg_exprs fun_dec.args in
    let _ = build_call f (Array.of_list args) "" cg_ctx.builder in
    false
  | {data=VoidReturn} ->
    build_ret_void cg_ctx.builder |> ignore;
    true
  | {data=Return(expr)} ->
    begin
      match ret_ty with
        | Some BaseET(bt,label) ->
          let ty = (bt_to_llvm_ty cg_ctx.llcontext bt.data) in
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

and declare_prototypes llcontext llmodule builder fenv = function
  | Lvalue lval,_ -> declare_lval_prototypes llcontext llmodule builder fenv lval.data
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

and declare_lval_prototypes cg_ctx llmodule builder fenv = function
  | Base _,_ -> ()
  | ArrayEl(_,expr),_ -> declare_prototypes cg_ctx llmodule builder fenv expr.data
  | StructEl _,_ -> ()

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
  ft'

let codegen_fun llcontext llmodule builder fenv sdecs verify_llvm = function
  | { data=FunDec(name,funattrs,ret,params,body) } ->
    Log.info "Generating function, %s" name.data;
    let venv = Env.new_env () in
    let tenv = Env.new_env () in
    let vtenv = Env.new_env () in
    let cg_ctx = { llcontext; llmodule; builder; venv; fenv; tenv; vtenv; sdecs; verify_llvm } in
    let ft = declare_prototype cg_ctx llmodule builder fenv params ret name in
      if not funattrs.export && not verify_llvm then
        set_linkage Internal ft;
      (match funattrs.inline with
        | Always ->
          add_function_attr ft Alwaysinline
        | Never ->
          add_function_attr ft Noinline
        | _ -> ());
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
    let cg_ctx = { llcontext; llmodule; builder; venv; fenv; tenv; vtenv; sdecs; verify_llvm } in
    declare_prototype cg_ctx llmodule builder fenv params ret_ty fun_name
  | { data=DebugFunDec _} -> raise CodegenError
  | { data=StdlibFunDec _} -> raise CodegenError

let rec codegen_fdecs llcontext llmodule builder fenv sdecs verify = function
  | [] -> ()
  | fd::rest ->
    ignore(codegen_fun llcontext llmodule builder fenv sdecs verify fd);
    codegen_fdecs llcontext llmodule builder fenv sdecs verify rest

let field_to_type llctx = function
  | {data=Field(var_name,
    {data=RefVT({data=base_type},maybe_label,_)})} ->
    bt_to_llvm_ty llctx base_type
  | {data=Field(var_name,{data=ArrayVT({data=ArrayAT(bt,size)} as ty,maybe_label,_)})} ->
    begin
      match size.data with
        | LIntLiteral s ->
          array_type (bt_to_llvm_ty llctx bt.data) s
    end

let codegen_sdec llctx {data=sdec} =
  let Struct(s,fields) = sdec in
  let field_tys = List.map (field_to_type llctx) fields |> Array.of_list in
  let struct_ty = named_struct_type llctx s.data in
    struct_set_body struct_ty field_tys true;
    (s.data, struct_ty)

let rec codegen llcontext llmodule builder verify = function
  | Module(oldfenv,fdecs,sdecs) ->
    Log.info "Codegening module";
    let sdecs = List.map (codegen_sdec llcontext) sdecs in
    let fenv = new_fenv oldfenv in
      codegen_fdecs llcontext llmodule builder fenv sdecs verify fdecs
