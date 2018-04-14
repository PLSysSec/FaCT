open Tast
open Pos
open Llvm

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

let p : pos = {file=""; line=0; lpos=0; rpos=0}
#define mkpos make_ast p @@

let load_le_proto' n name' =
  let name = mkpos name' in
  let ft = { export=false; inline=Always } in

  let rt' = mkpos BaseET(mkpos UInt n, mkpos Fixed Secret) in
  let rt = Some rt' in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Const) in
  let params = [mkpos Param (mkpos "src", arg)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
    name,fdec

let load_vec_le_proto' bw n name' =
  let name = mkpos name' in
  let ft = { export=false; inline=Always } in

  let rt' = mkpos BaseET(mkpos UVec(bw,n), mkpos Fixed Secret) in
  let rt = Some rt' in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (bw * n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Const) in
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
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Mut) in

  let w = mkpos RefVT(mkpos UInt n, mkpos Fixed Secret, mkpos Const) in
  let params = [mkpos Param (mkpos "dst", arg); mkpos Param (mkpos "w", w)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
    name,fdec

let store_vec_le_proto' bw n name' =
  let name = mkpos name' in
  let ft = { export=false; inline=Always } in
  let rt = None in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (bw * n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Mut) in

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
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Mut) in

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

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LDynamic (mkpos "_len")) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Mut) in

  let len = mkpos RefVT(mkpos UInt 32, mkpos Fixed Public, mkpos Const) in
  let params = [mkpos Param (mkpos "arr", arg); mkpos Param (mkpos "_len", len)] in

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

let functions = [
  load32_le_proto ();
  load64_le_proto ();
  load32_4_le_proto ();
  store32_le_proto ();
  store64_le_proto ();
  store32_4_le_proto ();
  memzero_proto ();
  memzero64_proto ();
]
