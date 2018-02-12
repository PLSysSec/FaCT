open Tast
open Pos
open Llvm

#define cerr(msg, p) InternalCompilerError("error: " ^ msg << p)
#define err(p) cerr("internal compiler error", p)

let p : pos = {file=""; line=0; lpos=0; rpos=0}
#define mkpos make_ast p @@

let load_le_proto () =
  let name = mkpos "_load_le" in
  let ft = { export=false; inline=Always } in

  let rt' = mkpos BaseET(mkpos UInt 32, mkpos Fixed Secret) in
  let rt = Some rt' in

  let arr = mkpos ArrayAT(mkpos UInt 8, mkpos LIntLiteral 4) in
  let arg = mkpos ArrayVT(arr, mkpos Fixed Secret, mkpos Const) in
  let params = [mkpos Param (mkpos "src", arg)] in

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
    set_linkage Internal fn;
  let bb = append_block llcontext "entry" fn in
  let b = builder llcontext in
    position_at_end bb b;
    let arr = param fn 0 in
    let cast = build_bitcast arr pty "_secret_cast" b in
    let load = build_load cast "_secret_load" b in
      build_ret load b;
      fn

let get_stdlib name llctx llmod =
  match name with
    | "_load_le" -> load_le_codegen llctx llmod

let functions = [
  load_le_proto ();
]
