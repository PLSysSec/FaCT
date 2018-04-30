open Llvm
open Tast
open Pos
open Codegen_utils

(*
  TODO:
    1) Make a local variable for each struct
*)

exception CTVerifError

type t =
  | ASSUME
  | PUBLIC_IN
  | PUBLIC_OUT
  | DECLASSIFIED_OUT
  | SMACK_VALUE
  | SMACK_VALUES
  | SMACK_RETURN_VALUE
  | DISJOINT_REGIONS

type c_code = string
(*
let string_of_ct_verif = function
  | ASSUME -> "__VERIFIER_assume"
  | PUBLIC_IN -> "public_in"
  | PUBLIC_OUT -> "public_out"
  | DECLASSIFIED_OUT -> "declassified_out"
  | SMACK_VALUE -> "__SMACK_value"
  | SMACK_VALUES -> "__SMACK_values"
  | SMACK_RETURN_VALUE -> "__SMACK_return_value"
  | DISJOINT_REGIONS -> "disjoint_regs"

let smack_ty = ref None

(* Todo: throw this into a type environment so that it can be reused *)
let smack_struct ctx llmod =
  let str = named_struct_type ctx "struct.smack_value" in
  let pt = pointer_type (i8_type ctx) in
  struct_set_body str [| pt |] false; (* TODO: what should the packing be?*)
  str

let get_smack_ty llctx llmod =
  match !smack_ty with
    | None ->
      let st = pointer_type (smack_struct llctx llmod) in
      smack_ty := Some st;
      st
    | Some st -> st

(* Target dependent attributes. These are for x86-64. We need to explore
   this more if we want to support more architectures *)
let set_attributes f =
  add_target_dependent_function_attr f "less-precise-fpmad" "false";
  add_target_dependent_function_attr f "no-frame-pointer-elim" "true";
  add_target_dependent_function_attr f "no-frame-pointer-elim-non-leaf" "";
  add_target_dependent_function_attr f "no-infs-fp-math" "false";
  add_target_dependent_function_attr f "no-nans-fp-math" "false";
  add_target_dependent_function_attr f "stack-protector-buffer-size" "8";
  add_target_dependent_function_attr f "target-cpu" "x86-64";
  add_target_dependent_function_attr f "target-features" "+fxsr,+mmx,+sse,+sse2";
  add_target_dependent_function_attr f "unsafe-fp-math" "false";
  add_target_dependent_function_attr f "use-soft-float" "false";
  add_target_dependent_function_attr f "disable-tail-calls" "false"

let declare_ct_verif verify_llvm llctx llmod keyword =
  if not verify_llvm then () else
  match keyword with
    | ASSUME ->
      let i32_ty = i32_type llctx in
      let vt = void_type llctx in
      let arg_types = [| i32_ty |] in
      let ft = function_type vt arg_types in
      let f = declare_function (string_of_ct_verif ASSUME) ft llmod in
      set_attributes f
    | PUBLIC_IN ->
      let smack_ty = get_smack_ty llctx llmod in
      let vt = void_type llctx in
      let ft = function_type vt [| smack_ty |] in
      let f = declare_function (string_of_ct_verif PUBLIC_IN) ft llmod in
      set_attributes f
    | PUBLIC_OUT ->
      let smack_ty = get_smack_ty llctx llmod in
      let vt = void_type llctx in
      let ft = function_type vt [| smack_ty |] in
      let f = declare_function (string_of_ct_verif PUBLIC_OUT) ft llmod in
      set_attributes f
    | DECLASSIFIED_OUT ->
      let smack_ty = get_smack_ty llctx llmod in
      let vt = void_type llctx in
      let ft = function_type vt [| smack_ty |] in
      let f = declare_function (string_of_ct_verif DECLASSIFIED_OUT) ft llmod in
      set_attributes f
    | SMACK_VALUE ->
      let smack_ty = get_smack_ty llctx llmod in
      let ft = var_arg_function_type smack_ty [||] in
      let f = declare_function (string_of_ct_verif SMACK_VALUE) ft llmod in
      set_attributes f
    | SMACK_VALUES ->
      let smack_ty = get_smack_ty llctx llmod in
      let i8_ptr_ty = pointer_type (i8_type llctx) in
      let i32_ty = i32_type llctx in
      let ft = function_type smack_ty [| i8_ptr_ty; i32_ty |] in
      let f = declare_function (string_of_ct_verif SMACK_VALUES) ft llmod in
      set_attributes f
    | SMACK_RETURN_VALUE ->
      let smack_ty = get_smack_ty llctx llmod in
      let ft = function_type smack_ty [||] in
      let f = declare_function (string_of_ct_verif SMACK_RETURN_VALUE) ft llmod in
      set_attributes f
    | DISJOINT_REGIONS ->
      let pt = pointer_type (i8_type llctx) in
      let st = i64_type llctx in
      let ft = function_type (void_type llctx) [| pt; st; pt; st |] in
      let f = declare_function (string_of_ct_verif DISJOINT_REGIONS) ft llmod in
      set_attributes f

let codegen_dec cg_ctx vt llvalue =
  let vt_to_et = function
      | ArrayVT(at,lab,mut',_) -> ArrayET(at,lab,mut')
      | _ -> raise CTVerifError
  in
  let extract_label = function
    | RefVT(bt,{data=Fixed(label)},_) ->
      let bt' = Codegen_utils.bt_to_llvm_ty cg_ctx.llcontext bt.data in
      Some(label,bt')
    | ArrayVT(_,{data=Fixed(label)},_,_) as vt ->
      let bt = expr_ty_to_base_ty (vt_to_et vt) in
      let ty = pointer_type (bt_to_llvm_ty cg_ctx.llcontext bt) in
      Some(label,ty)
    | _ -> None 
  in

  let public_in ty =
    let smack_ty = get_smack_ty cg_ctx.llcontext cg_ctx.llmodule in
      (* Bitcast the smack value *)
      let ret_ty = var_arg_function_type smack_ty [| ty; |] in
      let f = match lookup_function (string_of_ct_verif SMACK_VALUE) cg_ctx.llmodule with
        | None -> raise CTVerifError
        | Some f' -> f' in
      let cast = const_bitcast f (pointer_type ret_ty) in
      let v = build_call cast [|llvalue|] "" cg_ctx.builder in
      (* Call ct-verifs @public_in function *)
      let public_in = match lookup_function (string_of_ct_verif PUBLIC_IN) cg_ctx.llmodule with
        | None -> raise CTVerifError
        | Some public_in' -> public_in' in
      build_call public_in [| v |] "" cg_ctx.builder |> ignore
  in
  
  if not cg_ctx.verify_llvm then () else
  let label = extract_label vt.data in
  match label with
    | None -> ()
    | Some(Unknown,_) -> ()
    | Some(Secret,ty) ->
      begin match classify_type ty with
        | Llvm.TypeKind.Pointer ->  public_in ty
        | _ -> () end
    | Some(Public,ty) -> public_in ty
*)
let rec generate_combinations regions combinations =
  let rec generate_combinations' r rs acc = 
    match rs with
      | [] -> acc
      | first::rest -> generate_combinations' r rest ((r, first)::acc) in
  match regions with
    | [] -> List.flatten combinations
    | [r] -> List.flatten combinations
    | region1::region2::r ->
      let combinations' =
        generate_combinations' region1 (region2::r) [] in
      generate_combinations (region2::r) (combinations'::combinations)
(*
let generate_disjoint_regions verify_llvm regions cg_ctx arr_env =
  if not verify_llvm then () else
  let combinations = generate_combinations regions [] in
  let generate_len vn = function
    | LIntLiteral n -> const_int (i64_type cg_ctx.llcontext) n
    | LDynamic {data=""} ->
      let (vn : Tast.var_name),var = Env.find_var arr_env vn in
      let size = begin match var.data with
        | ArrayVT({data=ArrayAT(_,({data=LIntLiteral s}))},_,_,_) -> s
        | _ -> raise CTVerifError end in
        const_int (i64_type cg_ctx.llcontext) size
    | LDynamic var_name ->
      let var = Env.find_var cg_ctx.venv var_name in
      build_load var "len" cg_ctx.builder in
  let generate_disjoint_regions' ((r1,lvar1,vn),(r2,lvar2,vn')) =
    let l1 = generate_len vn lvar1 in
    let l2 = generate_len vn' lvar2 in
    let r1' = build_load r1 "r1" cg_ctx.builder in
    let r2' = build_load r2 "r2" cg_ctx.builder in
    let ty = pointer_type (i8_type cg_ctx.llcontext) in
    let lty = i64_type cg_ctx.llcontext in
    let r1'' = build_bitcast r1' ty "r1cast" cg_ctx.builder in
    let r2'' = build_bitcast r2' ty "r2cast" cg_ctx.builder in
    let l1' = build_sext l1 lty "l1sext" cg_ctx.builder in
    let l2' = build_sext l2 lty "l2sext" cg_ctx.builder in
    let fn = string_of_ct_verif DISJOINT_REGIONS in
    let callee = match lookup_function fn cg_ctx.llmodule with
      | Some fn -> fn
      | None -> raise CTVerifError in
    let args = [| r1''; l1' ; r2''; l2'|] in
    build_call callee args "" cg_ctx.builder |> ignore
    in
  List.iter generate_disjoint_regions' combinations

let declassify cg_ctx llval =
  if not cg_ctx.verify_llvm then () else
  let smack_ty = get_smack_ty cg_ctx.llcontext cg_ctx.llmodule in
  let ret_ty = var_arg_function_type smack_ty [| (type_of llval); |] in
  let f = match lookup_function (string_of_ct_verif SMACK_VALUE) cg_ctx.llmodule with
    | None -> raise CTVerifError
    | Some f' -> f' in
  let cast = const_bitcast f (pointer_type ret_ty) in
  let v = build_call cast [|llval|] "" cg_ctx.builder in
  let fname = string_of_ct_verif DECLASSIFIED_OUT in
  let declassify_out =
    match lookup_function fname cg_ctx.llmodule with
      | None -> raise CTVerifError
      | Some declassified_out -> declassified_out in
  build_call declassify_out [| v |] "" cg_ctx.builder |> ignore
*)
let name_of_param = function
  | { data=(Tast.Param(name,_)) } -> name.data

let rec vt_to_c_type sdecs vn = function
  | { data=RefVT({data=Int s},_,{ data=Mut })} ->
    "int" ^ (string_of_int s) ^ "_t *" ^ vn
  | { data=RefVT({data=UInt s},_,{ data=Mut })} ->
    "uint" ^ (string_of_int s) ^ "_t *" ^ vn
  | { data=RefVT({data=Bool},_,{ data=Mut })} ->
    "int32_t *" ^ vn
  | { data=RefVT({data=String},_,_)} ->
    "char *" ^ vn
  | { data=RefVT({data=Num(i,b)},_,{ data=Mut })} ->
    "int" ^ (string_of_int i) ^ "_t *" ^ vn
  | { data=RefVT({data=Int s},_,{ data=Const })} ->
    "int" ^ (string_of_int s) ^ "_t " ^ vn
  | { data=RefVT({data=UInt s},_,{ data=Const })} ->
    "uint" ^ (string_of_int s) ^ "_t " ^ vn
  | { data=RefVT({data=Bool},_,{ data=Const })} ->
    "int32_t " ^ vn
  | { data=RefVT({data=Num(i,b)},_,{ data=Const })} ->
    "int" ^ (string_of_int i) ^ "_t " ^ vn
  | { data=RefVT({data=UVec _},_,_)} ->
    raise CTVerifError
  | { data=ArrayVT({data=ArrayAT({data=Int s},_)},_,_,_) } ->
    "int" ^ (string_of_int s) ^ "_t *" ^ vn
  | { data=ArrayVT({data=ArrayAT({data=UInt s},_)},_,_,_) } ->
    "uint" ^ (string_of_int s) ^ "_t *" ^ vn
  | { data=ArrayVT({data=ArrayAT({data=Bool},_)},_,_,_) } ->
    "int32_t *" ^ vn
  | { data=ArrayVT({data=ArrayAT({data=String},_)},_,_,_) } ->
    raise CTVerifError
  | { data=ArrayVT({data=ArrayAT({data=Num(i,b)},_)},_,_,_) } ->
    "int" ^ (string_of_int i) ^ "_t *" ^ vn
  | { data=StructVT({ data=struct_name },{ data=_ })} ->
    let extract_field (Field (vn',vt,ip)) = vt_to_c_type sdecs vn'.data vt in
    let is_the_struct name = function
      | Struct(sn,_) when name = sn.data -> true
      | Struct(sn,_) -> false in
    let (Struct(sn,fields)) = List.find (is_the_struct struct_name) sdecs in
    let fields' = List.map (fun f -> extract_field f.data) fields in
    String.concat ", " fields'
  | _ -> raise CTVerifError

let fact_param_to_c_param sdecs = function
  | { data=Param(vn,vt) } -> vt_to_c_type sdecs vn.data vt

let rec extract_param_name sdecs = function
  | { data=Param(vn,{data=StructVT(sn,mut')})} ->
    let extract_field' = function
      | { data=Field(vn',({data=StructVT(sn',mut'')} as vt),ip); pos=p} ->
        extract_param_name sdecs {data=Param(vn',vt); pos=p};
      | { data=Field(vn',vt,ip)} -> vn'.data in
    let is_the_struct = function
      | Struct(sn',_) when sn'.data = sn.data -> true
      | Struct(sn',_) -> false in
    let (Struct(sn,fields)) = List.find is_the_struct sdecs in
    let args = List.map extract_field' fields in
    "{" ^ (String.concat ", " args) ^ "}" |> ignore;
    vn.data
  | { data=Param(vn,_) } ->
    let pre = Core.String.is_prefix vn.data ~prefix:"__" in
    let suf = Core.String.is_suffix vn.data ~suffix:"_len" in
    match pre, suf with
      | true, true -> "16"
      | _ -> vn.data

let build_structs sdecs = function
  | { data=Param(vn,{data=StructVT(sn,mut')})} ->
    let extract_field' = function
      | { data=Field(vn',({data=StructVT(sn',mut'')} as vt),ip); pos=p} ->
        extract_param_name sdecs {data=Param(vn',vt); pos=p};
      | { data=Field(vn',vt,ip)} -> vn'.data in
    let is_the_struct = function
      | Struct(sn',_) when sn'.data = sn.data -> true
      | Struct(sn',_) -> false in
    let (Struct(sn,fields)) = List.find is_the_struct sdecs in
    let args = List.map extract_field' fields in
    let s = "{" ^ (String.concat ", " args) ^ "}" in
    Log.error "Building struct %s %s = %s" sn.data vn.data s;
    Some ("struct " ^ sn.data ^ " " ^ vn.data ^ " = " ^ s ^ ";\n")
  | _ -> None

let build_function_top
  sdecs filename args fun_name public_args public_struct_fields disjoint_regions
  public_arrays pointers =
  let c_args = List.map (fact_param_to_c_param sdecs) args in
  let c_args' = String.concat ", " c_args in
  let includes = "#include \"ct-verif.h\"\n#include <stdint.h>\n#include \"" ^ filename ^ ".h\"\n" in
  let dec = "void " ^ fun_name ^ "_wrapper(" ^ c_args' ^ ") {\n" in
  let pointers = List.fold_left
    (fun s {data=Param(r1,_)} ->
      let pv = "public_in(__SMACK_value(" ^ r1.data ^ "));\n" in
      s ^ pv
    )
    dec pointers in
  let s' = List.fold_left
    (fun s p ->
      let pi = "public_in(__SMACK_value(" ^ (name_of_param p) ^ "));\n" in
      s ^ pi)
      pointers public_args in
  let s'' = List.fold_left
    (fun s {data=Field(vn,_,_)} ->
      let pi = "public_in(__SMACK_value(" ^ vn.data ^ "));\n" in
      s ^ pi)
    s' public_struct_fields in
  let s''' = List.fold_left
    (fun s (({data=Param(r1,_)},s1), ({data=Param(r2,_)},s2)) ->
      let dr = "__disjoint_regions(" ^ r1.data ^ "," ^ s1 ^
        "," ^ r2.data ^ "," ^ s2 ^ ");\n" in
      s ^ dr
    )
    s'' disjoint_regions in
  let public_values = List.fold_left
    (fun s ({data=Param(r1,_)}, size) ->
      let size = "16" in
      let pv = "public_in(__SMACK_values(" ^ r1.data ^ "," ^ size ^ "));\n" in
      s ^ pv
    )
    s''' public_arrays in
  let structs = List.map (build_structs sdecs) args in
  let structs' = List.filter Core.Option.is_some structs in
  let structs'' = List.fold_left
    (fun s (Some struct') ->
      s ^ struct'
    ) "" structs' in
  let arg_names = String.concat ", " (List.map (extract_param_name sdecs) args) in
  let fun_call = fun_name ^ "(" ^ arg_names ^ ");\n" in
  let s'''' = "\n}" in
  includes ^ public_values ^ structs'' ^ fun_call ^ s''''

let is_public = function
  | { data=Fixed(Public)} -> true
  | _ -> false

let is_arg_public = function
  | { data=(Param(name,{data=RefVT(bt,{ data=Fixed(Public) },mut')})) } -> true
  | { data=(Param(name,{data=ArrayVT(at,{ data=Fixed(Public)},mut',attr)})) } ->
    true
  | { data=(Param(name,{data=StructVT(sname,mut')})) } -> false
  | _ -> false

let public_struct_fields sdecs acc = function
  | { data=(Param(name,{data=StructVT(sn,mut')})) } ->
    let is_public_field = function
      | Field(vn,{data=RefVT(_,{data=Fixed Public},_)},_) -> true
      | Field(vn,{data=ArrayVT(_,{data=Fixed Public},_,_)},_) -> true
      | _ -> false in
    let is_the_struct = function
      | Struct(sn',_) when sn'.data = sn.data -> true
      | Struct(sn',_) -> false in
    let (Struct(sn,fields)) = List.find is_the_struct sdecs in
    let public_fields = List.filter (fun f -> is_public_field f.data) fields in
    acc @ public_fields
  | _ -> acc

let is_disjoint_region = function
  | { data=(Param(name,{data=ArrayVT(at,{ data=Fixed(_)},mut',attr)})) } ->
    true
  | _ -> false

let assign_array_size arr_env = function
  | { data=Param(_,
    { data=ArrayVT({ data=ArrayAT(_,{ data=LIntLiteral size})},_,_,_)})} as a ->
    (a,(string_of_int size))
  | { data=Param(vn,
    { data=ArrayVT({ data=ArrayAT(_,{ data=LDynamic{data=""}})},_,_,_)})} as a ->
    let (vn : Tast.var_name),var = Env.find_var arr_env vn in
    let size = begin match var.data with
      | ArrayVT({data=ArrayAT(_,({data=LIntLiteral s}))},_,_,_) -> s
      | _ -> raise CTVerifError end in
    (a,(string_of_int size))
  | { data=Param(_,
    { data=ArrayVT({ data=ArrayAT(_,{ data=LDynamic var_name})},_,_,_)})} as a ->
    (a,var_name.data)
  | _ -> raise CTVerifError

let is_public_array = function
  | { data=(Param(name,{data=ArrayVT(at,{ data=Fixed(Public)},mut',attr)})) } ->
    true
  | _ -> false

let is_array_or_mutable = function
  | { data=(Param(name,{data=ArrayVT(_)})) } -> true
  | { data=(Param(name,{data=RefVT(_,_,{data=Mut})}))} -> true
  | _ -> false

let generate_fdec_wrapper sdecs filename = function
  | FunDec(fn,{ export=true }, ret_ty, params, body) ->
    let arr_env,_ = body in
    let public_params = List.filter is_arg_public params in
    let public_arrays = List.filter is_public_array params in
    let pointers = List.filter is_array_or_mutable params in
    let public_arrays' = List.map (assign_array_size arr_env) public_arrays in
    let public_struct_fields =
      List.fold_left (public_struct_fields sdecs) [] params in
    let disjoint_regions = List.filter is_disjoint_region params in
    let disjoint_regions' =
      List.map (assign_array_size arr_env) disjoint_regions in
    let regions_combinations = generate_combinations disjoint_regions' [] in
    Some (fn,(build_function_top
      sdecs filename params fn.data public_params public_struct_fields
      regions_combinations public_arrays' pointers))
  | _ -> None

let generate_wrappers filename = function
  | Tast.Module (env, fdecs, structs) ->
    let sdecs = List.map (fun sdec -> sdec.data) structs in
    List.map (fun fdec -> generate_fdec_wrapper sdecs filename fdec.data) fdecs