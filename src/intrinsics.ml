open Llvm

let built : Llvm.llvalue -> unit = ignore

type intrinsic =
  | Memcpy
  | Memset
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

let rec get_intrinsic_code = function
  | SelectAsm8 n when n < 32 -> SelectAsm n
  | SelectAsm8 n -> SelectSel n
  | CmovAsm8 n when n < 32 -> CmovAsm n
  | CmovAsm8 n -> CmovSel n
  | _ as code -> code

let get_intrinsic_name = function
  | SelectAsm n -> "select.asm.i" ^ (string_of_int n)
  | SelectXor n -> "select.xor.i" ^ (string_of_int n)
  | SelectSel n -> "select.sel.i" ^ (string_of_int n)
  | CmovAsm n -> "cmov.asm.i" ^ (string_of_int n)
  | CmovXor n -> "cmov.xor.i" ^ (string_of_int n)
  | CmovSel n -> "cmov.sel.i" ^ (string_of_int n)

let make_stuff llctx llmod =
  let i1ty = i1_type llctx in
  let _i8ty = i8_type llctx in
  let _i16ty = i16_type llctx in
  let i32ty = i32_type llctx in
  let _i64ty = i64_type llctx in
  let _i128ty = integer_type llctx 128 in
  let _voidty = void_type llctx in
  let _noinline = create_enum_attr llctx "noinline" 0L in
  let alwaysinline = create_enum_attr llctx "alwaysinline" 0L in

  let define_intrinsic code =
    let name = get_intrinsic_name code in
      match code with
        | CmovAsm n
        | CmovXor n
        | CmovSel n ->
          let ity = integer_type llctx n in
          let asmty = if n < 32 then i32ty else ity in
          let ft = function_type ity [| i1ty; ity; ity |] in
          let asmfty = function_type asmty [| i1ty; asmty; asmty |] in
          let fn = define_function name ft llmod in
            add_function_attr fn alwaysinline Function;
            set_linkage Internal fn;
            let bb = entry_block fn in
            let b = builder llctx in
            let ext llval =
              if n < 32
              then build_zext llval i32ty "" b
              else llval in
            let trunc llval =
              if n < 32
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
                    | CmovXor _ ->
                      let m = build_sext cond ity "" b in
                      let xor = build_xor x y "" b in
                      let t = build_and m xor "" b in
                        build_xor y t "" b
                    | CmovSel _ ->
                      build_select cond x y "" b
                in
                  build_ret ret b |> built;
                  fn
        | SelectAsm n
        | SelectXor n
        | SelectSel n ->
          let ity = integer_type llctx n in
          let asmty = if n < 32 then i32ty else ity in
          let ft = function_type ity [| i1ty; ity; ity |] in
          let asmfty = function_type asmty [| i1ty; asmty; asmty |] in
          let fn = define_function name ft llmod in
            add_function_attr fn alwaysinline Function;
            set_linkage Internal fn;
            let bb = entry_block fn in
            let b = builder llctx in
            let ext llval =
              if n < 32
              then build_zext llval i32ty "" b
              else llval in
            let trunc llval =
              if n < 32
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
                    | SelectXor _ ->
                      let m = build_sext cond ity "" b in
                      let xor = build_xor x y "" b in
                      let t = build_and m xor "" b in
                        build_xor y t "" b
                    | SelectSel _ ->
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
