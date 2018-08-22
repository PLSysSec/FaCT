open Util
open Pos
open Err
open Tast
open Llvm

let ____ () = raise @@ err fake_pos

let built : Llvm.llvalue -> unit = ignore

class vardec_collector m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val mutable _vars : (var_name * base_type) list = []

    method _vars () =
      List.rev _vars

    method param param =
      begin
        match param.data with
          | Param (x,bty) ->
            _vars <- (x,bty) :: _vars
      end; super#param param

    method block_only (block,next) =
      begin
        match block.data with
          | RangeFor (x,bty,_,_,_)
          | ArrayFor (x,bty,_,_) ->
            _vars <- (x,bty) :: _vars
          | _ -> ()
      end; super#block_only (block,next)

    method stm stm =
      begin
        match stm.data with
          | VarDec (x,bty,_) ->
            _vars <- (x,bty) :: _vars
          | _ -> ()
      end; super#stm stm

  end

let collect_vardecs fdec =
  let m = Module([],[fdec],{fmap=[]}) in
  let visit = new vardec_collector m in
    visit#fact_module () |> ignore;
    visit#_vars ()

class codegen llctx llmod m =
  object (visit)
    val _b : Llvm.llbuilder = Llvm.builder llctx

    val _venv : (var_name * llvalue) mlist = ref []

    val i1ty = i1_type llctx
    val i8ty = i8_type llctx
    val i16ty = i16_type llctx
    val i32ty = i32_type llctx
    val i64ty = i64_type llctx
    val i128ty = integer_type llctx 128
    val voidty = void_type llctx
    val noinline = create_enum_attr llctx "noinline" 0L
    val alwaysinline = create_enum_attr llctx "alwaysinline" 0L

    method _get x =
      let thing = mlist_find ~equal:Tast_util.vequal !_venv x in
        match thing with
          | Some llval -> llval
          | None -> raise @@ err x.pos

    method bty {pos=p;data} =
      match data with
        | Bool _ -> i1ty
        | UInt (s,_) | Int (s,_) -> integer_type llctx s
        | Ref (bty,_) -> pointer_type (visit#bty bty)
        | Arr ({data=Ref (bty,_)},_,_) -> pointer_type (visit#bty bty)
        | Struct _ -> raise @@ err p
        | UVec (s,n,_) -> vector_type (integer_type llctx s) n
        | String -> raise @@ err p

    method fact_module () =
      let Module(sdecs,fdecs,minfo) = m in
      let _ = List.map visit#fdec (List.rev fdecs) in
        ()

    method _prototype name rt params =
      let param_types = List.map visit#param params |> Array.of_list in
      let ret_ty =
        match rt with
          | Some bty -> visit#bty bty
          | None -> voidty in
      function_type ret_ty param_types

    method fdec ({pos=p;data} as fdec) =
      match data with
        | FunDec(name,fnattr,rt,params,body) ->
          let ft = visit#_prototype name rt params in
          let llfn = declare_function name.data ft llmod in
            if not fnattr.export then
              set_linkage Internal llfn;
            begin
              match fnattr.inline with
                | Always ->
                  add_function_attr llfn alwaysinline Function
                | Never ->
                  add_function_attr llfn noinline Function
                | _ -> ()
            end;
            let bb = append_block llctx "entry" llfn in
              position_at_end bb _b;
              let vars = collect_vardecs fdec in
                List.iter
                  (fun (x,bty) ->
                     let llty = visit#bty bty in
                     let stackloc = build_alloca llty x.data _b in
                       mlist_push (x,stackloc) _venv)
                  vars;
                Array.iter2
                  (fun llparam {data=Param(x,_)} ->
                     let stackloc = visit#_get x in
                       build_store llparam stackloc _b |> built)
                  (Llvm.params llfn)
                  (Array.of_list params);
                visit#block body;
                llfn
        | CExtern _ -> ____()

    method param {pos=p;data} =
      match data with
        | Param (x,bty) ->
          visit#bty bty

    method block ({pos=p;data},next) =
      begin
        match data with
          | Scope _ -> ____()
          | ListOfStuff stms ->
            List.iter visit#stm stms
          | If (cond,thens,elses) -> ____()
          | RangeFor (x,bty,e1,e2,blk) -> ____()
          | ArrayFor (x,bty,e,blk) -> ____()
      end;
      visit#next next

    method next {pos=p;data} =
      match data with
        | Block blk -> visit#block blk
        | Return e ->
          let lle = visit#expr e in
            build_ret lle _b |> built
        | VoidReturn -> ____()
        | End -> ()

    method stm {pos=p;data} =
      match data with
        | VarDec (x,bty,e) ->
          let lle = visit#expr e in
          let loc = visit#_get x in
            build_store lle loc _b |> built
        | FnCall (x,bty,fn,args) -> ____()
        | VoidFnCall (fn,args) -> ____()
        | Assign (e1,e2) -> ____()
        | Cmov (e1,cond,e2) -> ____()
        | Assume e -> ____()

    method expr ({pos=p;data},bty) =
      let llbty = visit#bty bty in
        match data with
          | IntLiteral n -> const_int llbty n
          | Variable x ->
            let loc = visit#_get x in
              build_load loc "" _b

  end

let codegen m =
  let llctx = Llvm.create_context () in
  let llmod = Llvm.create_module llctx "Module" in
  let visit = new codegen llctx llmod m in
    visit#fact_module (); llctx, llmod
