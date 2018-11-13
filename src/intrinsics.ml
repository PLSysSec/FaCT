open Llvm

let built : Llvm.llvalue -> unit = ignore

type intrinsic =
  | Memcpy of int
  | Memset of int
  | SMemset of int
  | Rotl of int
  | Rotr of int
  | SelectAsm of int
  | SelectXor of int
  | SelectSel of int
  | SelectAsm8 of int
  | CmovAsm of int
  | CmovXor of int
  | CmovSel of int
  | CmovAsm8 of int

let select_of_choice n = SelectAsm n
let cmov_of_choice n = CmovAsm n

let rec get_intrinsic_code = function
  | SelectAsm8 sz when sz < 32 -> SelectAsm sz
  | SelectAsm8 sz -> SelectSel sz
  | CmovAsm8 sz when sz < 32 -> CmovAsm sz
  | CmovAsm8 sz -> CmovSel sz
  | _ as code -> code

let get_intrinsic_name = function
  | Memcpy sz -> "fact.memcpy.i" ^ (string_of_int sz)
  | Memset sz -> "fact.memset.i" ^ (string_of_int sz)
  | SMemset sz -> "fact.smemset.i" ^ (string_of_int sz)
  | Rotl sz -> "fact.rotl.i" ^ (string_of_int sz)
  | Rotr sz -> "fact.rotr.i" ^ (string_of_int sz)
  | SelectAsm sz -> "fact.select.asm.i" ^ (string_of_int sz)
  | SelectXor sz -> "fact.select.xor.i" ^ (string_of_int sz)
  | SelectSel sz -> "fact.select.sel.i" ^ (string_of_int sz)
  | CmovAsm sz -> "fact.cmov.asm.i" ^ (string_of_int sz)
  | CmovXor sz -> "fact.cmov.xor.i" ^ (string_of_int sz)
  | CmovSel sz -> "fact.cmov.sel.i" ^ (string_of_int sz)

let make_stuff llctx llmod =
  let i1ty = i1_type llctx in
  let i8ty = i8_type llctx in
  let _i16ty = i16_type llctx in
  let i32ty = i32_type llctx in
  let i64ty = i64_type llctx in
  let _i128ty = integer_type llctx 128 in
  let voidty = void_type llctx in
  let memty = pointer_type i8ty in
  let _noinline = create_enum_attr llctx "noinline" 0L in
  let alwaysinline = create_enum_attr llctx "alwaysinline" 0L in

  let define_intrinsic code =
    let name = get_intrinsic_name code in
    let def_internal name ft =
      let fn = define_function name ft llmod in
        add_function_attr fn alwaysinline Function;
        set_linkage Internal fn;
        let bb = entry_block fn in
        let b = builder llctx in
          position_at_end bb b;
          fn,b in
      match code with
        | Memcpy sz ->
          let bytesz = sz / 8 in
          let pty = pointer_type (integer_type llctx sz) in
          let memft = function_type voidty [| memty; memty; i64ty; i32ty; i1ty |] in
          let memcpy = declare_function "llvm.memcpy.p0i8.p0i8.i64" memft llmod in
          let ft = function_type voidty [| pty; pty; i64ty |] in
          let fn,b = def_internal name ft in
          let dst = param fn 0 in
          let src = param fn 1 in
          let len = param fn 2 in
          let alignment = const_int i32ty bytesz in
          let isvolatile = const_int i1ty 0 in
            set_value_name "dst" dst;
            set_value_name "src" src;
            set_value_name "len" len;
            let dst_ = build_bitcast dst memty "" b in
            let src_ = build_bitcast src memty "" b in
            let len_ = build_mul len (const_int i64ty bytesz) "" b in
              build_call memcpy [| dst_; src_; len_; alignment; isvolatile |] "" b |> built;
              build_ret_void b |> built;
              fn
        | Memset sz ->
          let bytesz = sz / 8 in
          let pty = pointer_type (integer_type llctx sz) in
          let memft = function_type voidty [| memty; i8ty; i64ty; i32ty; i1ty |] in
          let memset = declare_function "llvm.memset.p0i8.i64" memft llmod in
          let ft = function_type voidty [| pty; i8ty; i64ty |] in
          let fn,b = def_internal name ft in
          let dst = param fn 0 in
          let n = param fn 1 in
          let len = param fn 2 in
          let alignment = const_int i32ty bytesz in
          let isvolatile = const_int i1ty 0 in
            set_value_name "dst" dst;
            set_value_name "n" n;
            set_value_name "len" len;
            let dst_ = build_bitcast dst memty "" b in
            let len_ = build_mul len (const_int i64ty bytesz) "" b in
              build_call memset [| dst_; n; len_; alignment; isvolatile |] "" b |> built;
              build_ret_void b |> built;
              fn
        | SMemset sz ->
          let bytesz = sz / 8 in
          let pty = pointer_type (integer_type llctx sz) in
          let memft = function_type voidty [| memty; i8ty; i64ty; i32ty; i1ty |] in
          let memset = declare_function "llvm.memset.p0i8.i64" memft llmod in
          let ft = function_type voidty [| pty; i8ty; i64ty |] in
          let fn,b = def_internal name ft in
          let dst = param fn 0 in
          let n = param fn 1 in
          let len = param fn 2 in
          let alignment = const_int i32ty bytesz in
          let isvolatile = const_int i1ty 1 in
            set_value_name "dst" dst;
            set_value_name "n" n;
            set_value_name "len" len;
            let dst_ = build_bitcast dst memty "" b in
            let len_ = build_mul len (const_int i64ty bytesz) "" b in
              build_call memset [| dst_; n; len_; alignment; isvolatile |] "" b |> built;
              build_ret_void b |> built;
              fn
        | Rotl sz
        | Rotr sz ->
          (* we expect this function to get inlined and disappear at high optimization levels *)
          let ity = integer_type llctx sz in
          let ft = function_type ity [| ity; ity |] in
          let fn = define_function name ft llmod in
            add_function_attr fn alwaysinline Function;
            set_linkage Internal fn;
            let bb = entry_block fn in
            let b = builder llctx in
              position_at_end bb b;
              let e1 = param fn 0 in
              let e2 = param fn 1 in
                set_value_name "x" e1;
                set_value_name "n" e2;
                let build_fsh,build_bsh =
                  match code with
                    | Rotl _ -> build_shl,build_lshr
                    | Rotr _ -> build_lshr,build_shl in
                let shift1 = build_fsh e1 e2 "" b in
                let subtmp = build_sub (const_int ity sz) e2 "" b in
                let shift2 = build_bsh e1 subtmp "" b in
                let rotltmp = build_or shift1 shift2 "" b in
                  build_ret rotltmp b |> built;
                  fn
        | SelectAsm sz
        | SelectXor sz
        | SelectSel sz
        | CmovAsm sz
        | CmovXor sz
        | CmovSel sz ->
          let ity = integer_type llctx sz in
          let asmty = if sz < 32 then i32ty else ity in
          let ft = function_type ity [| i1ty; ity; ity |] in
          let asmfty = function_type asmty [| i1ty; asmty; asmty |] in
          let fn = define_function name ft llmod in
            add_function_attr fn alwaysinline Function;
            set_linkage Internal fn;
            let bb = entry_block fn in
            let b = builder llctx in
            let ext llval =
              if sz < 32
              then build_zext llval i32ty "" b
              else llval in
            let trunc llval =
              if sz < 32
              then build_trunc llval ity "" b
              else llval in
              position_at_end bb b;
              let cond = param fn 0 in
              let x = param fn 1 in
              let y = param fn 2 in
                set_value_name "cond" cond;
                set_value_name "x" x;
                set_value_name "y" y;
                let ret =
                  match code with
                    | SelectAsm _ ->
                      let x_ = ext x in
                      let y_ = ext y in
                      let asm = const_inline_asm
                                  asmfty
                                  "testb $1, $1; mov $3, $0; cmovnz $2, $0"
                                  "=&r,r,r,r,~{flags}"
                                  false false in
                      (* XXX measure speed/correctness on using =r instead of =&r *)
                      let ret_ = build_call asm [| cond; x_; y_ |] "" b in
                        trunc ret_
                    | CmovAsm _ ->
                      let x_ = ext x in
                      let y_ = ext y in
                      let asm = const_inline_asm
                                  asmfty
                                  "testb $1, $1; cmovnz $2, $0"
                                  "=r,r,r,0,~{flags}"
                                  false false in
                      let ret_ = build_call asm [| cond; x_; y_ |] "" b in
                        trunc ret_
                    | SelectXor _
                    | CmovXor _ ->
                      let m = build_sext cond ity "" b in
                      let xor = build_xor x y "" b in
                      let t = build_and m xor "" b in
                        build_xor y t "" b
                    | SelectSel _
                    | CmovSel _ ->
                      build_select cond x y "" b
                in
                  build_ret ret b |> built;
                  fn
  in

  let get_intrinsic code =
    let code = get_intrinsic_code code in
    let name = get_intrinsic_name code in
    let llfn = lookup_function name llmod in
      match llfn with
        | Some llfn -> llfn
        | None -> define_intrinsic code
  in
    get_intrinsic
