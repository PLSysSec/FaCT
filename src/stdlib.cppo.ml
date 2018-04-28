open Tast
open Pos
open Llvm

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

let p : pos = {file=""; line=0; lpos=0; rpos=0}
#define mkpos make_ast p @@

type intrinsic = 
  | Memcpy
  | Memset
  | Rotl of int
  | Rotr of int
  | CmovAsm of int
  | CmovXor of int
  | CmovSel of int
  | CmovAsm8 of int

let rec string_of_intrinsic = function
  | Memcpy -> "llvm.memcpy.p0i8.p0i8.i64"
  | Memset -> "llvm.memset.p0i8.i64"
  | Rotl n -> "__rotl" ^ (string_of_int n)
  | Rotr n -> "__rotr" ^ (string_of_int n)
  | CmovAsm n -> "select.cmov.asm.i" ^ (string_of_int n)
  | CmovXor n -> "select.cmov.xor.i" ^ (string_of_int n)
  | CmovSel n -> "select.cmov.sel.i" ^ (string_of_int n)
  | CmovAsm8 n ->
    if n < 32 then
      string_of_intrinsic (CmovAsm n)
    else
      string_of_intrinsic (CmovSel n)

let rec declare_intrinsic llctx llmod = function
  | Memcpy ->
    let i32_ty = i32_type llctx in
    let i64_ty = i64_type llctx in
    let ptr_ty = pointer_type (i8_type llctx) in
    let bool_ty = i1_type llctx in
    let arg_types = [| ptr_ty; ptr_ty; i64_ty; i32_ty; bool_ty |] in
    let vt = void_type llctx in
    let ft = function_type vt arg_types in
      declare_function (string_of_intrinsic Memcpy) ft llmod
  | Memset ->
    let i8_ty = i8_type llctx in
    let i32_ty = i32_type llctx in
    let i64_ty = i64_type llctx in
    let ptr_ty = pointer_type (i8_type llctx) in
    let bool_ty = i1_type llctx in
    let arg_types = [| ptr_ty; i8_ty; i64_ty; i32_ty; bool_ty |] in
    let vt = void_type llctx in
    let ft = function_type vt arg_types in
      declare_function (string_of_intrinsic Memset) ft llmod
  | Rotl n as rotl_sz ->
    (* we expect this function to get inlined and disappear at high optimization levels *)
    let ity = integer_type llctx n in
    let ft = function_type ity [| ity; ity |] in
    let fn = declare_function (string_of_intrinsic rotl_sz) ft llmod in
      add_function_attr fn Alwaysinline;
      set_linkage Internal fn;
    let bb = append_block llctx "entry" fn in
    let b = builder llctx in
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
    let ity = integer_type llctx n in
    let ft = function_type ity [| ity; ity |] in
    let fn = declare_function (string_of_intrinsic rotr_sz) ft llmod in
      add_function_attr fn Alwaysinline;
      set_linkage Internal fn;
    let bb = append_block llctx "entry" fn in
    let b = builder llctx in
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
  | CmovAsm n as cmov_sz ->
    let i1ty = i1_type llctx in
    let i32ty = i32_type llctx in
    let ity = integer_type llctx n in
    let asmty = if n < 32 then i32ty else ity in

    let ft = function_type ity [| i1ty; ity; ity |] in
    let asmfty = function_type asmty [| i1ty; asmty; asmty |] in

    let fn = declare_function (string_of_intrinsic cmov_sz) ft llmod in
      add_function_attr fn Alwaysinline;
      set_linkage Internal fn;
    let bb = append_block llctx "entry" fn in
    let b = builder llctx in

    let ext x' =
      if n < 32 then
        build_zext x' i32ty "_secret_zext" b
      else x' in
    let trunc x' =
      if n < 32 then
        build_trunc x' ity "_secret_trunc" b
      else x' in

      position_at_end bb b;
      let cond = param fn 0 in
      let x' = param fn 1 in
      let y' = param fn 2 in
        set_value_name "_secret_cond" cond;
        set_value_name "_secret_a" x';
        set_value_name "_secret_b" y';
        let x = ext x' in
        let y = ext y' in
        let asm = const_inline_asm
                    asmfty
                    "testb $1, $1; mov $3, $0; cmovnz $2, $0"
                    "=&r,r,r,r,~{flags}"
                    false false in
        let ret' = build_call asm [| cond; x; y; |] "_secret_asm" b in
        let ret = trunc ret' in
          build_ret ret b;
          fn
  | CmovXor n as cmov_sz ->
    let i1ty = i1_type llctx in
    let ity = integer_type llctx n in

    let ft = function_type ity [| i1ty; ity; ity |] in

    let fn = declare_function (string_of_intrinsic cmov_sz) ft llmod in
      add_function_attr fn Alwaysinline;
      set_linkage Internal fn;
    let bb = append_block llctx "entry" fn in
    let b = builder llctx in

      position_at_end bb b;
      let cond = param fn 0 in
      let x = param fn 1 in
      let y = param fn 2 in
        set_value_name "_secret_cond" cond;
        set_value_name "_secret_a" x;
        set_value_name "_secret_b" y;
        let m = build_sext cond ity "_secret_cond_sext" b in
        let xor = build_xor x y "_secret_xor" b in
        let t = build_and m xor "_secret_t" b in
        let ret = build_xor y t "_secret_res" b in
          build_ret ret b;
          fn
  | CmovSel n as cmov_sz ->
    let i1ty = i1_type llctx in
    let ity = integer_type llctx n in

    let ft = function_type ity [| i1ty; ity; ity |] in

    let fn = declare_function (string_of_intrinsic cmov_sz) ft llmod in
      add_function_attr fn Alwaysinline;
      set_linkage Internal fn;
    let bb = append_block llctx "entry" fn in
    let b = builder llctx in

      position_at_end bb b;
      let cond = param fn 0 in
      let x = param fn 1 in
      let y = param fn 2 in
        set_value_name "_secret_cond" cond;
        set_value_name "_secret_a" x;
        set_value_name "_secret_b" y;
        let ret = build_select cond x y "_secret_select" b in
          build_ret ret b;
          fn
  | CmovAsm8 n as cmov_sz ->
    if n < 32 then
      declare_intrinsic llctx llmod (CmovAsm n)
    else
      declare_intrinsic llctx llmod (CmovSel n)

let get_intrinsic intrinsic llctx llmod =
  match lookup_function (string_of_intrinsic intrinsic) llmod with
    | Some fn -> fn
    | None -> declare_intrinsic llctx llmod intrinsic


let load_le_proto' n name' =
  let name = mkpos name' in
  let ft = { export=false; inline=Always } in

  let rt' = mkpos BaseET(mkpos UInt n, mkpos Fixed Secret) in
  let rt = Some rt' in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Const, default_var_attr) in
  let params = [mkpos Param (mkpos "src", arg)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
    name,fdec

let load_vec_le_proto' bw n name' =
  let name = mkpos name' in
  let ft = { export=false; inline=Always } in

  let rt' = mkpos BaseET(mkpos UVec(bw,n), mkpos Fixed Secret) in
  let rt = Some rt' in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (bw * n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Const, default_var_attr) in
  let params = [mkpos Param (mkpos "src", arg)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
    name,fdec

let load32_le_proto () =
  load_le_proto' 32 "_load32_le"
let load64_le_proto () =
  load_le_proto' 64 "_load64_le"
let load32_4_le_proto () =
  load_vec_le_proto' 32 4 "_load32_4_le"

let store_le_proto' n name' =
  let name = mkpos name' in
  let ft = { export=false; inline=Always } in
  let rt = None in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Mut, default_var_attr) in

  let w = mkpos RefVT(mkpos UInt n, mkpos Fixed Secret, mkpos Const) in
  let params = [mkpos Param (mkpos "dst", arg); mkpos Param (mkpos "w", w)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
    name,fdec

let store_vec_le_proto' bw n name' =
  let name = mkpos name' in
  let ft = { export=false; inline=Always } in
  let rt = None in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (bw * n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Mut, default_var_attr) in

  let w = mkpos RefVT(mkpos UVec(bw,n), mkpos Fixed Secret, mkpos Const) in
  let params = [mkpos Param (mkpos "dst", arg); mkpos Param (mkpos "w", w)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
    name,fdec

let store32_le_proto () =
  store_le_proto' 32 "_store32_le"
let store64_le_proto () =
  store_le_proto' 64 "_store64_le"
let store32_4_le_proto () =
  store_vec_le_proto' 32 4 "_store32_4_le"

let memzero_proto' n name' () =
  let name = mkpos name' in
  let ft = { export=false; inline=Never } in
  let rt = None in

  let arr = mkpos ArrayAT(mkpos UInt n, mkpos LDynamic (mkpos "_len")) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Mut, default_var_attr) in

  let len = mkpos RefVT(mkpos UInt 32, mkpos Fixed Public, mkpos Const) in
  let params = [mkpos Param (mkpos "arr", arg); mkpos Param (mkpos "_len", len)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
    name,fdec

let memzero_proto = memzero_proto' 8 "_memzero"
let memzero64_proto = memzero_proto' 64 "_memzero64"

let arrcopy_proto () =
  let name = mkpos "_arrcopy" in
  let ft = { export=false; inline=Never } in
  let rt = None in

  let arr1 = mkpos ArrayAT(mkpos UInt 8, mkpos LDynamic (mkpos "_len1")) in
  let arr2 = mkpos ArrayAT(mkpos UInt 8, mkpos LDynamic (mkpos "_len2")) in
  let arg1 = mkpos ArrayVT(arr1, mkpos Fixed Secret, mkpos Mut, default_var_attr) in
  let arg2 = mkpos ArrayVT(arr2, mkpos Fixed Secret, mkpos Const, default_var_attr) in

  let len = mkpos RefVT(mkpos UInt 32, mkpos Fixed Public, mkpos Const) in
  let params = [ mkpos Param (mkpos "arr1", arg1);
                 mkpos Param (mkpos "_len1", len);
                 mkpos Param (mkpos "arr2", arg2);
                 mkpos Param (mkpos "_len2", len); ] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
  name,fdec

let load_le_codegen' n name llcontext llmodule =
  let ity = integer_type llcontext n in
  let bty = i8_type llcontext in
  let aty = pointer_type bty in
  let pty = pointer_type ity in
  let ft = function_type ity [| aty |] in
  let fn = declare_function name ft llmodule in
    add_function_attr fn Alwaysinline;
    add_function_attr fn Readonly;
    set_linkage Internal fn;
  let bb = append_block llcontext "entry" fn in
  let b = builder llcontext in
    position_at_end bb b;
    let arr = param fn 0 in
      add_param_attr arr Noalias;
      add_param_attr arr (Alignment 4);
    let cast = build_bitcast arr pty "_secret_cast" b in
    let load = build_load cast "_secret_load" b in
      build_ret load b;
      fn

let load_vec_le_codegen' bw n name llcontext llmodule =
  let ity = integer_type llcontext bw in
  let vty = vector_type ity n in
  let bty = i8_type llcontext in
  let aty = pointer_type bty in
  let pty = pointer_type vty in
  let ft = function_type vty [| aty |] in
  let fn = declare_function name ft llmodule in
    add_function_attr fn Alwaysinline;
    add_function_attr fn Readonly;
    set_linkage Internal fn;
  let bb = append_block llcontext "entry" fn in
  let b = builder llcontext in
    position_at_end bb b;
    let arr = param fn 0 in
      add_param_attr arr Noalias;
      add_param_attr arr (Alignment 4);
    let cast = build_bitcast arr pty "_secret_cast" b in
    let load = build_load cast "_secret_load" b in
      build_ret load b;
      fn

let load32_le_codegen =
  load_le_codegen' 32 "_load32_le"
let load64_le_codegen =
  load_le_codegen' 64 "_load64_le"
let load32_4_le_codegen =
  load_vec_le_codegen' 32 4 "_load32_4_le"

let store_le_codegen' n name llcontext llmodule =
  let ity = integer_type llcontext n in
  let bty = i8_type llcontext in
  let aty = pointer_type bty in
  let pty = pointer_type ity in
  let ft = function_type (void_type llcontext) [| aty; ity |] in
  let fn = declare_function name ft llmodule in
    add_function_attr fn Alwaysinline;
    set_linkage Internal fn;
  let bb = append_block llcontext "entry" fn in
  let b = builder llcontext in
    position_at_end bb b;
    let arr = param fn 0 in
    let w = param fn 1 in
    let cast = build_bitcast arr pty "_secret_cast" b in
      build_store w cast b;
      build_ret_void b;
      fn

let store_vec_le_codegen' bw n name llcontext llmodule =
  let ity = integer_type llcontext bw in
  let vty = vector_type ity n in
  let bty = i8_type llcontext in
  let aty = pointer_type bty in
  let pty = pointer_type vty in
  let ft = function_type (void_type llcontext) [| aty; vty |] in
  let fn = declare_function name ft llmodule in
    add_function_attr fn Alwaysinline;
    set_linkage Internal fn;
  let bb = append_block llcontext "entry" fn in
  let b = builder llcontext in
    position_at_end bb b;
    let arr = param fn 0 in
    let w = param fn 1 in
    let cast = build_bitcast arr pty "_secret_cast" b in
      build_store w cast b;
      build_ret_void b;
      fn

let store32_le_codegen =
  store_le_codegen' 32 "_store32_le"
let store64_le_codegen =
  store_le_codegen' 64 "_store64_le"
let store32_4_le_codegen =
  store_vec_le_codegen' 32 4 "_store32_4_le"

let memzero_codegen' n name llcontext llmodule =
  let i8_ty = i8_type llcontext in
  let i32_ty = i32_type llcontext in
  let ptr_ty = pointer_type i8_ty in
  let bool_ty = i1_type llcontext in
  let arg_types = [| ptr_ty; i8_ty; i32_ty; i32_ty; bool_ty |] in
  let vt = void_type llcontext in
  let ft = function_type vt arg_types in
  let memset = declare_function ("llvm.memset.p0i8.i32") ft llmodule in

  let ity = integer_type llcontext n in
  let iptr_ty = pointer_type ity in
  let arg_types = [| iptr_ty; i32_ty; |] in
  let vt = void_type llcontext in
  let ft = function_type vt arg_types in
  let fn = declare_function name ft llmodule in
    add_function_attr fn Noinline;
    set_linkage Internal fn;
  let bb = append_block llcontext "entry" fn in
  let b = builder llcontext in
    position_at_end bb b;
    let arr = param fn 0 in
    let len = param fn 1 in
    let zero = const_int i8_ty 0 in
    let alignment = const_int i32_ty (n / 8) in
    let arr' = build_bitcast arr ptr_ty "_secret_cast" b in
    let volatility = const_int bool_ty 0 in
    let args = [| arr'; zero; len; alignment; volatility |] in
      build_call memset args "" b;
      build_ret_void b;
      fn

let memzero_codegen = memzero_codegen' 8 "_memzero"
let memzero64_codegen = memzero_codegen' 64 "_memzero64"

let arrcopy_codegen llctx llmod =
  let i8_ty = i8_type llctx in
  let i32_ty = i32_type llctx in
  let ptr_ty = pointer_type i8_ty in
  let bool_ty = i1_type llctx in
  let arg_types = [| ptr_ty; ptr_ty; i32_ty; i32_ty; bool_ty; |] in
  let vt = void_type llctx in
  let ft = function_type vt arg_types in
  let memcpy = declare_function ("llvm.memcpy.p0i8.p0i8.i32") ft llmod in

  let arg_types = [| ptr_ty; i32_ty; ptr_ty; i32_ty; |] in
  let vt = void_type llctx in
  let ft = function_type vt arg_types in
  let fn = declare_function "_arrcopy" ft llmod in
    add_function_attr fn Alwaysinline;
    set_linkage Internal fn;
  let bb = append_block llctx "entry" fn in
  let b = builder llctx in
    position_at_end bb b;
    let arr1 = param fn 0 in
    let len = param fn 1 in
    let arr2 = param fn 2 in
    let alignment = const_int i32_ty 1 in
    let volatility = const_int bool_ty 0 in
    let args = [| arr1; arr2; len; alignment; volatility |] in
      build_call memcpy args "" b;
      build_ret_void b;
      fn

let get_stdlib name llctx llmod =
  match name with
    | "_load32_le" -> load32_le_codegen llctx llmod
    | "_load64_le" -> load64_le_codegen llctx llmod
    | "_load32_4_le" -> load32_4_le_codegen llctx llmod
    | "_store32_le" -> store32_le_codegen llctx llmod
    | "_store64_le" -> store64_le_codegen llctx llmod
    | "_store32_4_le" -> store32_4_le_codegen llctx llmod
    | "_memzero" -> memzero_codegen llctx llmod
    | "_memzero64" -> memzero64_codegen llctx llmod
    | "_arrcopy" -> arrcopy_codegen llctx llmod

let functions = [
  load32_le_proto ();
  load64_le_proto ();
  load32_4_le_proto ();
  store32_le_proto ();
  store64_le_proto ();
  store32_4_le_proto ();
  memzero_proto ();
  memzero64_proto ();
  arrcopy_proto ();
]
