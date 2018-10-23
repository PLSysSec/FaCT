open Util
open Pos
open Err
open Tast
open Tast_util

let sprintf = Printf.sprintf

let rmem sz len lbl =
  let p = fake_pos in
    p@>Param (p@>"src",
              p@>Arr (p@>Ref (p@>UInt (sz,lbl),p@>R),p@>LIntLiteral len,default_var_attr))

let wmem sz len lbl =
  let p = fake_pos in
    p@>Param (p@>"dst",
              p@>Arr (p@>Ref (p@>UInt (sz,lbl),p@>W),p@>LIntLiteral len,default_var_attr))

let rmem_unspec sz lbl =
  let p = fake_pos in
    [ p@>Param (p@>"src",
                p@>Arr (p@>Ref (p@>UInt (sz,lbl),p@>R),p@>LDynamic (p@>"srclen"),default_var_attr)) ;
      p@>Param (p@>"srclen",
                p@>UInt (64,p@>Public)) ]

let wmem_unspec sz lbl =
  let p = fake_pos in
    [ p@>Param (p@>"dst",
                p@>Arr (p@>Ref (p@>UInt (sz,lbl),p@>W),p@>LDynamic (p@>"dstlen"),default_var_attr)) ;
      p@>Param (p@>"dstlen",
                p@>UInt (64,p@>Public)) ]

let contains fn =
  match fn.data with
    | "memzero" -> true
    | "smemzero" -> true
    | "memcpy" -> true
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
            sprintf "__memzero[%d]_%s%s" sz (ps_lbl lbl) (if everhi then "/oblivious" else "")
          | SMemzero (sz,lbl,everhi) ->
            sprintf "__smemzero[%d]_%s%s" sz (ps_lbl lbl) (if everhi then "/oblivious" else "")
          | Memcpy (sz,lbl,everhi) ->
            sprintf "__memcpy[%d]_%s%s" sz (ps_lbl lbl) (if everhi then "/oblivious" else "")
          | MemzeroStruct (sname,everhi) ->
            sprintf "__memzero_%s%s" sname (if everhi then "/oblivious" else "")
          | SMemzeroStruct (sname,everhi) ->
            sprintf "__smemzero_%s%s" sname (if everhi then "/oblivious" else "")
          | MemcpyStruct (sname,everhi) ->
            sprintf "__memcpy_%s%s" sname (if everhi then "/oblivious" else "")
          | LoadLE (sz,lbl) ->
            sprintf "__load[%d]_%s_le" sz (ps_lbl lbl)
          | StoreLE (sz,lbl,everhi) ->
            sprintf "__store[%d]_%s%s_le" sz (ps_lbl lbl) (if everhi then "/oblivious" else "")
          | LoadLEVec (sz,len,lbl) ->
            sprintf "__load[%d]<%d>_%s_le" sz len (ps_lbl lbl)
          | StoreLEVec (sz,len,lbl,everhi) ->
            sprintf "__store[%d]<%d>_%s%s_le" sz len (ps_lbl lbl) (if everhi then "/oblivious" else "")
      end

let interface_of
      (tc_expr : ?lookahead_bty:Tast.base_type -> Ast.expr -> Tast.expr)
      p stmlbl fn args =
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
          begin match (type_of arg').data with
            | Arr ({data=Ref (subty,{data=W|RW})},lexpr,_) ->
              let sz,lbl = match subty.data with
                | UInt (s,l) -> s,l
                | _ -> raise @@ err p in
              let rt' = None in
              let params' = wmem_unspec sz lbl in
              let arglen = p@>Ast.ArrayLen arg in
              let args' = [ arg; arglen ] in
              let fdec' = fake_pos @> StdlibFn (Memzero (sz,lbl.data,everhi),fnattr,rt',params') in
                fdec',args'
            | Struct sname ->
              let p = fake_pos in
              let rt' = None in
              let params' = [ p@>Param (p@>"dst", p@>Ref(p@>Struct sname, p@>W)) ] in
              let args' = [ arg ] in
              let fdec' = fake_pos @> StdlibFn (MemzeroStruct (sname.data,everhi),fnattr,rt',params') in
                fdec',args'
            | _ -> raise @@ err p
          end

      | "smemzero" ->
        let arg = match args with
          | [arg] -> arg
          | _ -> raise @@ err p in
        let arg' = tc_expr arg in
          begin match (type_of arg').data with
            | Arr ({data=Ref (subty,{data=W|RW})},lexpr,_) ->
              let sz,lbl = match subty.data with
                | UInt (s,l) -> s,l
                | _ -> raise @@ err p in
              let rt' = None in
              let params' = wmem_unspec sz lbl in
              let arglen = p@>Ast.ArrayLen arg in
              let args' = [ arg; arglen ] in
              let fdec' = fake_pos @> StdlibFn (SMemzero (sz,lbl.data,everhi),fnattr,rt',params') in
                fdec',args'
            | Struct sname ->
              let p = fake_pos in
              let rt' = None in
              let params' = [ p@>Param (p@>"dst", p@>Ref(p@>Struct sname, p@>W)) ] in
              let args' = [ arg ] in
              let fdec' = fake_pos @> StdlibFn (SMemzeroStruct (sname.data,everhi),fnattr,rt',params') in
                fdec',args'
            | _ -> raise @@ err p
          end

      | "memcpy" ->
        let arg1,arg2 = match args with
          | [arg1;arg2] -> arg1,arg2
          | _ -> raise @@ err p in
        let arg1' = tc_expr arg1 in
        let arg2' = tc_expr arg2 in
        begin match (type_of arg1').data with
          | Arr _ ->
            let subty1,lexpr1 = match (type_of arg1').data with
              | Arr ({data=Ref (subty,{data=W|RW})},lexpr,_) -> subty,lexpr
              | _ -> raise @@ err p in
            let subty2,lexpr2 = match (type_of arg2').data with
              | Arr ({data=Ref (subty,{data=R|RW})},lexpr,_) -> subty,lexpr
              | _ -> raise @@ err p in
            let _ = match lexpr1.data,lexpr2.data with
              | LIntLiteral n,LIntLiteral m when n = m -> ()
              | LDynamic x,LDynamic y when vequal x y -> ()
              | _ -> raise @@ cerr p "unequal lengths to memcpy" in
            let sz1,lbl1 = match subty1.data with
              | UInt (s,l) -> s,l
              | _ -> raise @@ err p in
            let sz2,lbl2 = match subty2.data with
              | UInt (s,l)
                when s = sz1 && l <$ lbl1 -> s,l
              | _ -> raise @@ err p in
            let lbl = lbl1 +$ lbl2 in
            let rt' = None in
            let params1' = wmem_unspec sz1 lbl1 in
            let params2' = rmem_unspec sz2 lbl2 in
            let params' = (List.hd params1') :: params2' in
            let arglen2 = p@>Ast.ArrayLen arg2 in
            let args' = [ arg1; arg2; arglen2 ] in
            let fdec' = fake_pos @> StdlibFn (Memcpy (sz1,lbl.data,everhi),fnattr,rt',params') in
              fdec',args'
          | Struct sname ->
            let p = fake_pos in
            let rt' = None in
            let params1' = p@>Param (p@>"dst", p@>Ref(p@>Struct sname, p@>W)) in
            let params2' = p@>Param (p@>"src", p@>Ref(p@>Struct sname, p@>R)) in
            let params' = [ params1'; params2' ] in
            let args' = [ arg1; arg2 ] in
            let fdec' = fake_pos @> StdlibFn (MemcpyStruct (sname.data,everhi),fnattr,rt',params') in
              fdec',args'
          | _ -> raise @@ err p
        end

      | "load_le" ->
        let arg = match args with
          | [arg] -> arg
          | _ -> raise @@ err p in
        let arg' = tc_expr arg in
        let subty,lexpr = match (type_of arg').data with
          | Arr ({data=Ref (subty,{data=R|RW})},lexpr,_) -> subty,lexpr
          | _ -> raise @@ err p in
        let vecbase,lbl = match subty.data with
          | UInt (8,l) -> 0,l
          | UInt (s,l) -> s,l
          | _ -> raise @@ err p in
        let sz_ = match lexpr.data with
          | LIntLiteral sz -> sz
          | _ -> raise @@ err p in
        let base,rt',code = match vecbase with
          | 0 ->
            let base = 8 in
            let sz = sz_ * 8 in
            let rt' = Some (fake_pos @> UInt (sz,lbl)) in
            let code = LoadLE (sz,lbl.data) in
              base,rt',code
          | _ ->
            let base = vecbase in
            let rt' = Some (fake_pos @> UVec (vecbase,sz_,lbl)) in
            let code = LoadLEVec (vecbase,sz_,lbl.data) in
              base,rt',code in
        let params' = [ rmem base sz_ lbl ] in
        let args' = [ arg ] in
        let fdec' = fake_pos @> StdlibFn (code,fnattr,rt',params') in
          fdec',args'

      | "store_le" ->
        let arg1,arg2 = match args with
          | [arg1;arg2] -> arg1,arg2
          | _ -> raise @@ err p in
        let arg1' = tc_expr arg1 in
        let subty,lexpr = match (type_of arg1').data with
          | Arr ({data=Ref (subty,{data=W|RW})},lexpr,_) -> subty,lexpr
          | _ -> raise @@ err p in
        let vecbase,lbl = match subty.data with
          | UInt (8,l) -> 0,l
          | UInt (s,l) -> s,l
          | _ -> raise @@ err p in
        let sz_ = match lexpr.data with
          | LIntLiteral sz -> sz
          | _ -> raise @@ err p in
        let base,valty,code = match vecbase with
          | 0 ->
            let base = 8 in
            let sz = sz_ * 8 in
            let valty = p@>UInt (sz,lbl) in
            let code = StoreLE (sz,lbl.data,everhi) in
              base,valty,code
          | _ ->
            let base = vecbase in
            let valty = p@>UVec (vecbase,sz_,lbl) in
            let code = StoreLEVec (vecbase,sz_,lbl.data,everhi) in
              base,valty,code
        in
        let arg2' = tc_expr ~lookahead_bty:(element_type valty >!> valty) arg2 in
        let arg2ty = type_of arg2' in
          if not (arg2ty <: valty) then
            raise @@ err p;
          let rt' = None in
          let params' = [ wmem base sz_ lbl ;
                          fake_pos@>Param (fake_pos@>"val", valty) ] in
          let args' = [ arg1; arg2 ] in
          let fdec' = fake_pos @> StdlibFn (code,fnattr,rt',params') in
            fdec',args'

      | _ -> raise @@ cerr p "not a stdlib function: '%s'" fn.data

let llvm_for
      (sget : struct_name -> Llvm.lltype)
      llctx llmod code =
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
          raise @@ cerr fake_pos "oblivious memzero not yet implemented"

        | SMemzero (sz,_,false) ->
          let pty = pointer_type (integer_type llctx sz) in
          let ft = function_type voidty [| pty; i64ty |] in
          let fn,b = def_internal name.data ft in
          let zero = const_null i8ty in
          let memset = _get_intrinsic (SMemset sz) in
          let dst = param fn 0 in
          let len = param fn 1 in
            set_value_name "dst" dst;
            set_value_name "len" len;
            build_call memset [| dst; zero; len |] "" b |> built;
            build_ret_void b |> built;
            fn
        | SMemzero (sz,_,true) ->
          raise @@ cerr fake_pos "oblivious smemzero not yet implemented"

        | Memcpy (sz,_,false) ->
          let pty = pointer_type (integer_type llctx sz) in
          let ft = function_type voidty [| pty; pty; i64ty |] in
          let fn,b = def_internal name.data ft in
          let memcpy = _get_intrinsic (Intrinsics.Memcpy sz) in
          let dst = param fn 0 in
          let src = param fn 1 in
          let len = param fn 2 in
            set_value_name "dst" dst;
            set_value_name "src" src;
            set_value_name "len" len;
            build_call memcpy [| dst; src; len |] "" b |> built;
            build_ret_void b |> built;
            fn
        | Memcpy (sz,_,true) ->
          raise @@ cerr fake_pos "oblivious memcpy not yet implemented"

        | MemzeroStruct (s,false) ->
          let llstruct = sget (fake_pos @> s) in
          let pty = pointer_type llstruct in
          let ft = function_type voidty [| pty |] in
          let fn,b = def_internal name.data ft in
          let zero = const_null i8ty in
          let memset = _get_intrinsic (Memset 8) in
          let dst = param fn 0 in
            set_value_name "dst" dst;
            let dst_ = build_bitcast dst memty "" b in
            let len = size_of llstruct in
              build_call memset [| dst_; zero; len |] "" b |> built;
              build_ret_void b |> built;
              fn
        | MemzeroStruct (s,true) ->
          raise @@ cerr fake_pos "oblivious memzero not yet implemented"

        | SMemzeroStruct (s,false) ->
          let llstruct = sget (fake_pos @> s) in
          let pty = pointer_type llstruct in
          let ft = function_type voidty [| pty |] in
          let fn,b = def_internal name.data ft in
          let zero = const_null i8ty in
          let memset = _get_intrinsic (SMemset 8) in
          let dst = param fn 0 in
            set_value_name "dst" dst;
            let dst_ = build_bitcast dst memty "" b in
            let len = size_of llstruct in
              build_call memset [| dst_; zero; len |] "" b |> built;
              build_ret_void b |> built;
              fn
        | SMemzeroStruct (s,true) ->
          raise @@ cerr fake_pos "oblivious smemzero not yet implemented"

        | MemcpyStruct (s,false) ->
          let llstruct = sget (fake_pos @> s) in
          let pty = pointer_type llstruct in
          let ft = function_type voidty [| pty; pty |] in
          let fn,b = def_internal name.data ft in
          let memcpy = _get_intrinsic (Intrinsics.Memcpy 8) in
          let dst = param fn 0 in
          let src = param fn 1 in
            set_value_name "dst" dst;
            set_value_name "src" src;
            let dst_ = build_bitcast dst memty "" b in
            let src_ = build_bitcast src memty "" b in
            let len = size_of llstruct in
              build_call memcpy [| dst_; src_; len |] "" b |> built;
              build_ret_void b |> built;
              fn
        | MemcpyStruct (s,true) ->
          raise @@ cerr fake_pos "oblivious memcpy not yet implemented"

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

        | LoadLEVec (sz,len,_) ->
          let ity = integer_type llctx sz in
          let vty = vector_type ity len in
          let pty = pointer_type vty in
          let ft = function_type vty [| pointer_type ity |] in
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

        | StoreLEVec (sz,len,_,false) ->
          let ity = integer_type llctx sz in
          let vty = vector_type ity len in
          let pty = pointer_type vty in
          let ft = function_type voidty [| pointer_type ity; vty |] in
          let fn,b = def_internal name.data ft in
          let dst = param fn 0 in
          let value = param fn 1 in
            set_value_name "dst" dst;
            set_value_name "value" value;
            let cast = build_bitcast dst pty "" b in
              build_store value cast b |> built;
              build_ret_void b |> built;
              fn
        | StoreLEVec (sz,len,_,true) ->
          let ity = integer_type llctx sz in
          let vty = vector_type ity len in
          let pty = pointer_type vty in
          let ft = function_type voidty [| pointer_type ity; vty; i1ty |] in
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
