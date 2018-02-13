open Tast
open Pos
open Llvm

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

let p : pos = {file=""; line=0; lpos=0; rpos=0}
#define mkpos make_ast p @@

let load_le_proto () =
  let n = 32 in
  let name = mkpos "_load_le" in
  let ft = { export=false; inline=Always } in

  let rt' = mkpos BaseET(mkpos UInt n, mkpos Fixed Secret) in
  let rt = Some rt' in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Const) in
  let params = [mkpos Param (mkpos "src", arg)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
  name,fdec

let store_le_proto () =
  let n = 32 in
  let name = mkpos "_store_le" in
  let ft = { export=false; inline=Always } in

  let rt = None in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral (n / 8)) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Mut) in

  let w = mkpos RefVT(mkpos UInt n, mkpos Fixed Secret, mkpos Const) in
  let params = [mkpos Param (mkpos "dst", arg); mkpos Param (mkpos "w", w)] in

  let fdec = mkpos (StdlibFunDec(name,ft,rt,params)) in
  name,fdec

let load_le_codegen llcontext llmodule =
  let n = 32 in
  let ity = integer_type llcontext n in
  let bty = i8_type llcontext in
  let aty = pointer_type bty in
  let pty = pointer_type ity in
  let ft = function_type ity [| aty |] in
  let fn = declare_function "_load_le" ft llmodule in
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

let store_le_codegen llcontext llmodule =
  let n = 32 in
  let ity = integer_type llcontext n in
  let bty = i8_type llcontext in
  let aty = pointer_type bty in
  let pty = pointer_type ity in
  let ft = function_type (void_type llcontext) [| aty; ity |] in
  let fn = declare_function "_store_le" ft llmodule in
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

let get_stdlib name llctx llmod =
  match name with
    | "_load_le" -> load_le_codegen llctx llmod
    | "_store_le" -> store_le_codegen llctx llmod

let functions = [
  load_le_proto ();
  store_le_proto ();
]
