open Util
open Pos
open Err
open Tast
open Tast_util

let sprintf = Printf.sprintf

let rmem8 len lbl =
  let p = fake_pos in
    p@>Param (p@>"src",
              p@>Arr (p@>Ref (p@>UInt (8,lbl),p@>R),p@>LIntLiteral len,default_var_attr))

let wmem8 len lbl =
  let p = fake_pos in
    p@>Param (p@>"dst",
              p@>Arr (p@>Ref (p@>UInt (8,lbl),p@>W),p@>LIntLiteral len,default_var_attr))

let wmem sz lbl =
  let p = fake_pos in
    [ p@>Param (p@>"dst",
                p@>Arr (p@>Ref (p@>UInt (sz,lbl),p@>W),p@>LDynamic (p@>"len"),default_var_attr)) ;
      p@>Param (p@>"len",
                p@>UInt (64,p@>Public)) ]

let contains fn =
  match fn.data with
    | "memzero" -> true
    | "load_le" -> true
    | "store_le" -> true
    | _ -> false

let name_of code =
  let ps_lbl = function
    | Public -> "public"
    | Secret -> "secret" in
    make_ast fake_pos
      begin
        match code with
          | Memzero (sz,lbl,everhi) ->
            sprintf "__memzero[%d]/%s%s" sz (ps_lbl lbl) (if everhi then "/oblivious" else "")
          | LoadLE (sz,lbl) ->
            sprintf "__load[%d]/%s_le" sz (ps_lbl lbl)
          | StoreLE (sz,lbl,everhi) ->
            sprintf "__store[%d]/%s%s_le" sz (ps_lbl lbl) (if everhi then "/oblivious" else "")
      end

let interface_of (tc_expr : ?lookahead_bty:Tast.base_type -> Ast.expr -> Tast.expr) p stmlbl fn args =
  let everhi = match stmlbl.data with
    | Public -> false
    | Secret -> true in
  let fnattr = { export=false; inline=Always; everhi } in
    match fn.data with

      | "memzero" ->
        let arg = match args with
          | [arg] -> arg
          | _ -> raise @@ err p in
        let arg' = tc_expr arg in
        let subty,lexpr = match (type_of arg').data with
          | Arr ({data=Ref (subty,{data=W|RW})},lexpr,_) -> subty,lexpr
          | _ -> raise @@ err p in
        let sz,lbl = match subty.data with
          | UInt (s,l) -> s,l
          | _ -> raise @@ err p in
        let rt' = None in
        let params' = wmem sz lbl in
        let arglen = p@>Ast.ArrayLen arg in
        let args' = [ arg; arglen ] in
        let fdec' = fake_pos @> StdLibFn (Memzero (sz,lbl.data,everhi),fnattr,rt',params') in
          fdec',args'

      | "load_le" ->
        let arg = match args with
          | [arg] -> arg
          | _ -> raise @@ err p in
        let arg' = tc_expr arg in
        let subty,lexpr = match (type_of arg').data with
          | Arr ({data=Ref (subty,{data=R|RW})},lexpr,_) -> subty,lexpr
          | _ -> raise @@ err p in
        let lbl = match subty.data with
          | UInt (8,l) -> l
          | _ -> raise @@ err p in
        let sz_ = match lexpr.data with
          | LIntLiteral sz -> sz
          | _ -> raise @@ err p in
        let sz = sz_ * 8 in
        let rt' = Some (fake_pos @> UInt (sz,lbl)) in
        let params' = [ rmem8 sz_ lbl ] in
        let args' = [ arg ] in
        let fdec' = fake_pos @> StdLibFn (LoadLE (sz,lbl.data),fnattr,rt',params') in
          fdec',args'

      | "store_le" ->
        let arg1,arg2 = match args with
          | [arg1;arg2] -> arg1,arg2
          | _ -> raise @@ err p in
        let arg1' = tc_expr arg1 in
        let subty,lexpr = match (type_of arg1').data with
          | Arr ({data=Ref (subty,{data=W|RW})},lexpr,_) -> subty,lexpr
          | _ -> raise @@ err p in
        let lbl = match subty.data with
          | UInt (8,l) -> l
          | _ -> raise @@ err p in
        let sz_ = match lexpr.data with
          | LIntLiteral sz -> sz
          | _ -> raise @@ err p in
        let sz = sz_ * 8 in
        let usz = p@>UInt (sz,lbl) in
        let arg2' = tc_expr ~lookahead_bty:usz arg2 in
        let _ = match (type_of arg2').data with
          | UInt (sz2,lbl2) when sz = sz2 && lbl2 <$ lbl -> ()
          | _ -> raise @@ err p in
        let rt' = None in
        let params' = [ wmem8 sz_ lbl ;
                        fake_pos@>Param (fake_pos@>"val", fake_pos@>UInt (sz,lbl)) ] in
        let args' = [ arg1; arg2 ] in
        let fdec' = fake_pos @> StdLibFn (StoreLE (sz,lbl.data,everhi),fnattr,rt',params') in
          fdec',args'

      | _ -> raise @@ cerr p "not a stdlib function: '%s'" fn.data

let llvm_for llctx llmod code =
  Llvm.(
    let i1ty = i1_type llctx in
    let i8ty = i8_type llctx in
    let _i16ty = i16_type llctx in
    let _i32ty = i32_type llctx in
    let i64ty = i64_type llctx in
    let _i128ty = integer_type llctx 128 in
    let voidty = void_type llctx in
    let memty = pointer_type i8ty in
    let _noinline = create_enum_attr llctx "noinline" 0L in
    let alwaysinline = create_enum_attr llctx "alwaysinline" 0L in
    let _get_intrinsic = Intrinsics.make_stuff llctx llmod in

    let built : Llvm.llvalue -> unit = ignore in

    let def_internal name ft =
      let fn = define_function name ft llmod in
        add_function_attr fn alwaysinline Function;
        set_linkage Internal fn;
        let bb = entry_block fn in
        let b = builder llctx in
          position_at_end bb b;
          fn,b in

    let name = name_of code in

      match code with

        | Memzero (sz,_,false) ->
          let pty = pointer_type (integer_type llctx sz) in
          let ft = function_type voidty [| pty; i64ty |] in
          let fn,b = def_internal name.data ft in
          let zero = const_null i8ty in
          let memset = _get_intrinsic (Memset sz) in
          let dst = param fn 0 in
          let len = param fn 1 in
            set_value_name "dst" dst;
            set_value_name "len" len;
            build_call memset [| dst; zero; len |] "" b |> built;
            build_ret_void b |> built;
            fn
        | Memzero (sz,_,true) ->
          let pty = pointer_type (integer_type llctx sz) in
          let ft = function_type voidty [| pty; i64ty; i1ty |] in
          let fn,b = def_internal name.data ft in
          let _zero = const_null i8ty in
          let _memset = _get_intrinsic (Memset sz) in
          let dst = param fn 0 in
          let len = param fn 1 in
          let fctx = param fn 2 in
            set_value_name "dst" dst;
            set_value_name "len" len;
            set_value_name "fctx" fctx;
            raise @@ cerr fake_pos "oblivious memzero not yet implemented"

        | LoadLE (sz,_) ->
          let ity = integer_type llctx sz in
          let pty = pointer_type ity in
          let ft = function_type ity [| memty |] in
          let fn,b = def_internal name.data ft in
          let src = param fn 0 in
            set_value_name "src" src;
            let cast = build_bitcast src pty "" b in
            let load = build_load cast "" b in
              build_ret load b |> built;
              fn

        | StoreLE (sz,_,false) ->
          let ity = integer_type llctx sz in
          let pty = pointer_type ity in
          let ft = function_type voidty [| memty; ity |] in
          let fn,b = def_internal name.data ft in
          let dst = param fn 0 in
          let value = param fn 1 in
            set_value_name "dst" dst;
            set_value_name "value" value;
            let cast = build_bitcast dst pty "" b in
              build_store value cast b |> built;
              build_ret_void b |> built;
              fn
        | StoreLE (sz,_,true) ->
          let ity = integer_type llctx sz in
          let pty = pointer_type ity in
          let ft = function_type voidty [| memty; ity; i1ty |] in
          let fn,b = def_internal name.data ft in
          let cmov = _get_intrinsic (Intrinsics.cmov_of_choice sz) in
          let dst = param fn 0 in
          let value = param fn 1 in
          let fctx = param fn 2 in
            set_value_name "dst" dst;
            set_value_name "value" value;
            set_value_name "fctx" fctx;
            let cast = build_bitcast dst pty "" b in
            let load = build_load cast "" b in
            let sel = build_call cmov [| fctx; value; load |] "" b in
              build_store sel cast b |> built;
              build_ret_void b |> built;
              fn
  )
