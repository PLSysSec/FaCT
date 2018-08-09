open Util
open Pos
open Err
open Tast
open Tast_util

let get p = function
  | Some x -> x
  | None -> raise @@ err p

let findvar vmap x =
  match Core.List.Assoc.find vmap x ~equal:vequal with
    | Some bty -> bty
    | None -> raise @@ cerr x.pos
                         "variable not defined: '%s'"
                         x.data

let findfn fmap fname =
  match Core.List.Assoc.find fmap fname ~equal:vequal with
    | Some fnty -> fnty
    | None -> raise @@ cerr fname.pos
                         "function not defined: '%s'"
                         fname.data


class typechecker =
  object (visit)
    val mutable _vmap : (var_name * base_type) list = []
    val mutable _fmap : (fun_name * (ret_type * params)) list = []
    val mutable _cur_rt : ret_type = None
    val mutable _inject : statement list = []

    method fact_module m =
      let Ast.Module(sdecs,fdecs) = m in
      let sdecs' = List.map visit#sdec sdecs in
        (* pre-scan function defs for out-of-order definitions *)
        List.iter
          (fun fdec ->
             match fdec.data with
               | Ast.FunDec(fn,_,rt,params,_)
               | Ast.CExtern(fn,rt,params) ->
                 let rt' = rt >>= visit#bty ?lookahead_lexpr:None in
                   begin
                     match rt' with
                       | Some {data=(Bool _ | UInt _ | Int _)}
                       | None -> ()
                       | _ -> raise @@ err fdec.pos
                   end;
                   let params' = List.map visit#param params in
                     _fmap <- (fn,(rt',params')) :: _fmap)
          fdecs;
        let fdecs' = List.map visit#fdec fdecs in
          Module(sdecs',fdecs')

    method sdec =
      wrap @@ fun p -> function
        | Ast.StructDef (name,fields) ->
          StructDef (name,List.map visit#field fields)

    method inline = function
      | Ast.Default -> Default
      | Ast.Always  -> Always
      | Ast.Never   -> Never

    method fntype Ast.{ export; inline } =
      { export; inline=visit#inline inline; everhi=false }

    method lbl =
      wrap @@ fun p -> function
        | Ast.Public -> Public
        | Ast.Secret -> Secret

    method basic l =
      wrap @@ fun p -> function
        | Ast.BaseBool -> Bool l
        | Ast.BaseUInt s -> UInt (s,l)
        | Ast.BaseInt s -> Int (s,l)

    method mut =
      wrap @@ fun p -> function
        | Ast.R -> R
        | Ast.W -> W
        | Ast.RW -> RW

    method vattr Ast.{ cache_aligned } =
      { cache_aligned }

    method field =
      wrap @@ fun p -> function
        | Ast.Field (x,bty) -> Field (x,visit#bty bty)

    (* Note: we explicitly do not recursively pass down the lookahead *)
    method bty ?lookahead_lexpr {pos=p; data} =
      make_ast p begin
        match data with
          | Ast.Bool l -> Bool (visit#lbl l)
          | Ast.UInt (s,l) -> UInt (s,visit#lbl l)
          | Ast.Int (s,l) -> Int (s,visit#lbl l)
          | Ast.Ref (bty,m) -> Ref (visit#bty bty,visit#mut m)
          | Ast.Arr (bty,lexpr,vattr) -> Arr (visit#bty bty,visit#lexpr ?lookahead_lexpr lexpr,visit#vattr vattr)
          | Ast.Struct fields -> Struct (List.map visit#field fields)
          | Ast.UVec (s,n,l) -> UVec (s,n,visit#lbl l)
          | Ast.String -> String
      end

    method fdec =
      wrap @@ fun p -> function
        | Ast.FunDec(fn,ft,rt,params,body) ->
          let ft' = visit#fntype ft in
          let rt' = rt >>= visit#bty in
          let params' = List.map visit#param params in
            _cur_rt <- rt';
            let body' = visit#block body in
              FunDec(fn,ft',rt',params',body')
        | Ast.CExtern (fn,rt,params) ->
          let params' = List.map visit#param params in
            CExtern(fn,rt >>= visit#bty,params')

    method param =
      wrap @@ fun p -> function
        | Ast.Param(x,bty) -> Param(x,visit#bty bty)

    method block blk =
      visit#stms blk

    method stms stms_ =
      stms_ |> List.map visit#stm |> List.flatten

    method lexpr ?lookahead_lexpr {pos=p; data} =
      make_ast p begin
        match data with
          | Ast.LIntLiteral n -> LIntLiteral n
          | Ast.LExpression e ->
            let e' = visit#expr e in
            let e_bty = type_of e' in
            let e_lbl = label_of e_bty in
              if not @@ is_integral e_bty then
                raise @@ err p;
              if e_lbl.data <> Public then
                raise @@ err p;
              let x = p @> make_fresh "lexpr" in
              let var_dec = p@>VarDec(x,e_bty,e') in
                _inject <- var_dec :: _inject;
                LDynamic x
          | Ast.LUnspecified -> (get p lookahead_lexpr).data
      end

    method expr ?lookahead_bty e_ =
      let {data=(e',bty); pos} = visit#expr' lookahead_bty e_ in
        (make_ast pos e', bty)

    method _ternop p e1 e2 e3 =
      let e1' = visit#expr e1 in
      let e2',e3' =
        match Ast_util.is_untyped_int e2 with
          | Some _ ->
            let e3' = visit#expr e3 in
            let e2' = visit#expr ~lookahead_bty:(type_of e3') e2 in
              e2',e3'
          | None ->
            let e2' = visit#expr e2 in
            let e3' = visit#expr ~lookahead_bty:(type_of e2') e3 in
              e2',e3' in
      let bty1 = type_of e1' in
      let bty2 = type_of e2' in
      let bty3 = type_of e3' in
        if not (is_bool bty1) then
          raise @@ err p;
        if not (bty2 <: bty3 || bty3 <: bty2) then
          raise @@ err p;
        let l1 = label_of bty1 in
        let new_bty = bty2 +: bty3 in
        let new_bty =
          match l1.data with
            | Public -> new_bty
            | Secret -> p @>
              begin
                match new_bty.data with
                  | Bool _ -> Bool l1
                  | UInt (n,_) -> UInt (n,l1)
                  | Int (n,_) -> Int (n,l1)
                  | UVec (n,bw,_) -> UVec (n,bw,l1)
              end
        in
          (e1',e2',e3'), new_bty

    method expr' lookahead_bty =
      wrap @@ fun p -> function
        | Ast.True -> True, make_ast p @@ Bool (make_ast p Public)
        | Ast.False -> False, make_ast p @@ Bool (make_ast p Public)
        | Ast.UntypedIntLiteral n ->
          let bty = get p lookahead_bty in
            if not @@ is_integral bty then
              raise @@ err p;
            IntLiteral n, bty
        | Ast.IntLiteral (n,bty) ->
          let bty' = visit#basic (make_ast p Public) bty in
            if not @@ is_integral bty' then
              raise @@ err p;
            IntLiteral n, bty'
        | Ast.Variable x ->
          let bty = findvar _vmap x in
            Variable x, bty
        | Ast.ArrayLen e ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
          let lexpr =
            match e_bty.data with
              | Arr (_,lexpr,_) -> lexpr
              | _ -> raise @@ err p in
            begin
              match lexpr.data with
                | LIntLiteral n -> IntLiteral n, p@>UInt (64, p@>Public)
                | LDynamic x ->
                  let x_bty = findvar _vmap x in
                    Variable x, x_bty
            end
        | Ast.Cast (bty,e) ->
          let lookahead_bty = visit#basic (p@>Public) bty in
          let e' = visit#expr ~lookahead_bty e in
          let e_bty = type_of e' in
          let e_lbl = label_of e_bty in
          let c_bty = visit#basic (label_of e_bty) bty in
          let c_lbl = label_of c_bty in
            if not (is_integral e_bty || is_bool e_bty) then
              raise @@ err p;
            if not (is_integral c_bty || is_bool c_bty) then
              raise @@ err p;
            if not (e_lbl <$ c_lbl) then
              raise @@ err p;
            Cast (c_bty,e'), c_bty
        | Ast.UnOp (op,e) ->
          let e' = visit#expr e in
            UnOp(op,e'), visit#unop op e'
        | Ast.BinOp (op,e1,e2) ->
          let e1',e2' =
            match Ast_util.is_untyped_int e1 with
              | Some _ ->
                let e2' = visit#expr e2 in
                let e1' = visit#expr ~lookahead_bty:(type_of e2') e1 in
                  e1',e2'
              | None ->
                let e1' = visit#expr e1 in
                let e2' = visit#expr ~lookahead_bty:(type_of e1') e2 in
                  e1',e2'
          in
            BinOp(op,e1',e2'), visit#binop op e1' e2'
        | Ast.TernOp (e1,e2,e3) ->
          let (e1',e2',e3'),new_bty = visit#_ternop p e1 e2 e3 in
            TernOp(e1',e2',e3'), new_bty
        | Ast.Select (e1,e2,e3) ->
          let (e1',e2',e3'),new_bty = visit#_ternop p e1 e2 e3 in
            Select(e1',e2',e3'), new_bty
        | Ast.Declassify e ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
          let pub = e_bty.pos@>Public in
          let rec pub_bty bty_ =
            let newty =
              match bty_.data with
                | Bool _ -> Bool pub
                | UInt (s,_) -> UInt (s,pub)
                | Int (s,_) -> Int (s,pub)
                | UVec (s,bw,_) -> UVec (s,bw,pub)
                | Ref (bty,m) -> Ref (pub_bty bty,m)
                | Arr (bty,lexpr,vattr) -> Arr (pub_bty bty,lexpr,vattr)
                | _ -> raise @@ err p in
              bty_.pos @> newty
          in
            Declassify e', pub_bty e_bty
        | Ast.Enref e ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
            Enref e', p@>Ref (e_bty, p@>RW)
        | Ast.Deref e ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
          let unref_bty =
            match e_bty.data with
              | Ref (bty,{data=R|RW}) -> bty
              | _ -> raise @@ err p in
            Deref e', unref_bty
        | Ast.ArrayGet (e,index) ->
          let e' = visit#expr e in
          let index' = visit#lexpr index in
          let e_bty = type_of e' in
          let el_bty =
            match e_bty.data with
              | Arr ({data=Ref (bty,{data=R|RW})},_,_)
              | Arr (bty,_,_) -> bty
              | _ -> raise @@ err p in
            ArrayGet (e', index'), el_bty
        | Ast.ArrayLit es ->
          if List.length es = 0 then
            raise @@ err p;
          let es' = List.map visit#expr es in
          let btys = List.map type_of es' in
          let bty = Core.List.fold btys ~init:(List.hd btys) ~f:(+:) in
            ArrayLit es', bty
        | Ast.ArrayZeros lexpr ->
          let bty = get p lookahead_bty in
            begin
              match bty.data with
                | Arr ({data=Ref (bty,_)},_,_)
                | Arr (bty,_,_) ->
                  if not @@ is_integral bty then
                    raise @@ err p;
                | _ -> raise @@ err p
            end;
            ArrayZeros (visit#lexpr lexpr), bty
        | Ast.ArrayCopy e ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
            begin
              match e_bty.data with
                | Arr _ -> ()
                | _ -> raise @@ err p
            end;
            ArrayCopy e', e_bty
        | Ast.ArrayView (e,index,len) ->
          let e' = visit#expr e in
          let index' = visit#lexpr index in
          let len' = visit#lexpr len in
          let e_bty = type_of e' in
          let new_ty =
            match e_bty.data with
              | Arr (el_ty,_,_) -> Arr (el_ty,len',default_var_attr)
              | _ -> raise @@ err p in
            ArrayView (e',index',len'), e_bty.pos @> new_ty
        | Ast.Shuffle (e,ns) ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
          let new_bw = List.length ns in
          let new_ty =
            match e_bty.data with
              | UVec (s,bw,l) ->
                if new_bw > bw then
                  raise @@ err p;
                List.iter
                  (fun n ->
                     if n < 0 || n >= bw then
                       raise @@ err p)
                  ns;
                begin
                  match new_bw with
                    | 0 -> raise @@ err p
                    | 1 -> UInt (s,l)
                    | _ -> UVec (s,new_bw,l)
                end
              | _ -> raise @@ err p in
            Shuffle (e', ns), e_bty.pos @> new_ty
        | Ast.StructLit entries ->
          (* XXX pre-parse the fieldnames to match to a struct for lookaheading *)
          let entries' = List.map
                           (fun (field,e) ->
                              (field,visit#expr e))
                           entries in
          let fields = List.map
                         (fun (field,e) ->
                            field.pos @> Field (field,type_of e))
                         entries' in
            StructLit entries', p@>Struct fields
        | Ast.StructGet (e,field) ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
          let f_bty =
            begin
              match e_bty.data with
                | Struct fields ->
                  List.find_opt
                    (fun {data=Field (fieldname,f_bty)} ->
                       vequal field fieldname)
                    fields >>= fun x ->
                  let {data=Field (_,f_bty)} = x in
                    f_bty
                | _ -> None
            end >!!> err p
          in
            StructGet (visit#expr e,field), f_bty
        | Ast.StringLiteral _ -> raise @@ cerr p "strings are not implemented yet"

    method unop op e =
      let bty = type_of e in
        match op with
          | Ast.Neg ->
            if not @@ is_integral bty then
              raise @@ err bty.pos;
            bty
          | Ast.LogicalNot ->
            if not @@ is_bool bty then
              raise @@ err bty.pos;
            bty
          | Ast.BitwiseNot ->
            if not @@ is_integral bty then
              raise @@ err bty.pos;
            bty

    method binop op e1 e2 =
      let bty1 = type_of e1 in
      let bty2 = type_of e2 in
        match op with
          | Ast.Plus
          | Ast.Minus
          | Ast.Multiply
          | Ast.Divide
          | Ast.Modulo ->
            if not @@ (is_integral bty1 || is_vec bty1) then
              raise @@ err bty1.pos;
            if not @@ (is_integral bty2 || is_vec bty2) then
              raise @@ err bty2.pos;
            if not (bty1 <: bty2) then
              raise @@ err bty2.pos;
            bty1 +: bty2
          | Ast.Equal
          | Ast.NEqual ->
            if not (is_integral bty1 || is_bool bty1 || is_vec bty1) then
              raise @@ err bty1.pos;
            if not (is_integral bty2 || is_bool bty2 || is_vec bty2) then
              raise @@ err bty2.pos;
            if not (bty1 <: bty2 || bty2 <: bty1) then
              raise @@ err bty1.pos;
            let l1 = label_of bty1 in
            let l2 = label_of bty2 in
              bty1.pos@>Bool (l1 +$ l2)
          | Ast.GT
          | Ast.GTE
          | Ast.LT
          | Ast.LTE ->
            if not (is_integral bty1) then
              raise @@ err bty1.pos;
            if not (is_integral bty2) then
              raise @@ err bty2.pos;
            if not (bty1 <: bty2 || bty2 <: bty1) then
              raise @@ err bty1.pos;
            let l1 = label_of bty1 in
            let l2 = label_of bty2 in
              bty1.pos@>Bool (l1 +$ l2)
          | Ast.LogicalAnd
          | Ast.LogicalOr ->
            if not (is_bool bty1) then
              raise @@ err bty1.pos;
            if not (is_bool bty2) then
              raise @@ err bty2.pos;
            bty1 +: bty2
          | Ast.BitwiseAnd ->
            begin
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (m,l2) ->
                  bty1.pos@>UInt (min n m,l1 +$ l2)
                | Int (n,l1),Int (m,l2) ->
                  bty1.pos@>Int (min n m,l1 +$ l2)
                | UVec (n,bw1,l1),UVec (m,bw2,l2)
                  when n = m && bw1 = bw2 ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err bty1.pos
            end
          | Ast.BitwiseOr
          | Ast.BitwiseXor ->
            begin
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (m,l2) ->
                  bty1.pos@>UInt (max n m,l1 +$ l2)
                | Int (n,l1),Int (m,l2) ->
                  bty1.pos@>Int (max n m,l1 +$ l2)
                | UVec (n,bw1,l1),UVec (m,bw2,l2)
                  when n = m && bw1 = bw2 ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err bty1.pos
            end
          | Ast.LeftShift
          | Ast.RightShift ->
            begin
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (_,l2) ->
                  bty1.pos@>UInt (n,l1 +$ l2)
                | Int (n,l1),UInt (_,l2) ->
                  bty1.pos@>Int (n,l1 +$ l2)
                | UVec (n,bw1,l1),UInt (_,l2) ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err bty1.pos
            end
          | Ast.LeftRotate
          | Ast.RightRotate ->
            begin
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (_,l2) ->
                  bty1.pos@>UInt (n,l1 +$ l2)
                | _ -> raise @@ err bty1.pos
            end

    method _fncall p expected_rt fn args =
      let rt,params = findfn _fmap fn in
        if not (List.length args <> List.length params) then
          raise @@ err p;
        let args' =
          List.map2
            (fun arg param ->
               let Param (_,param_ty) = param.data in
                 visit#expr ~lookahead_bty:param_ty arg)
            args params in
          List.iter2
            (fun arg param ->
               let arg_ty = type_of arg in
               let Param (_,param_ty) = param.data in
                 if not (arg_ty <: param_ty) then
                   raise @@ err p)
            args' params;
          begin
            match expected_rt,rt with
              | Some ert,Some rt ->
                if not (rt <: ert) then
                  raise @@ err p
              | None,None -> ()
              | _ -> raise @@ err p
          end;
          args'

    method stm stm_ =
      let p = stm_.pos in
      let stm' =
        match stm_.data with
          | Ast.Block blk ->
            Block (visit#block blk)
          | Ast.VarDec (x,bty,e) ->
            let e',e_bty,bty' =
              if Ast_util.is_unspec_arr bty then
                let e' = visit#expr e in
                let e_bty = type_of e' in
                  begin
                    match e_bty.data with
                      | Arr (_,lexpr,_) ->
                        let bty' = visit#bty ~lookahead_lexpr:lexpr bty in
                          e',e_bty,bty'
                      | _ ->
                        raise @@ err p
                  end
              else
                let bty' = visit#bty bty in
                let e' = visit#expr ~lookahead_bty:bty' e in
                let e_bty = type_of e' in
                  e',e_bty,bty'
            in
              if not (e_bty <: bty') then
                raise @@ err p;
              _vmap <- (x,bty') :: _vmap;
              VarDec (x,bty',e')
          | Ast.FnCall (x,bty,fn,args) ->
            let bty' = visit#bty bty in
            let args' = visit#_fncall p (Some bty') fn args in
              _vmap <- (x,bty') :: _vmap;
              FnCall (x,bty',fn,args')
          | Ast.VoidFnCall (fn,args) ->
            let args' = visit#_fncall p None fn args in
              VoidFnCall (fn,args')
          | Ast.Assign (e1,e2) ->
            let e1' = visit#expr e1 in
            let e1_ty = type_of e1' in
            let unref_ty =
              match e1_ty.data with
                | Ref (bty,{data=W|RW}) -> bty
                | _ -> raise @@ err p in
            let e2' = visit#expr ~lookahead_bty:unref_ty e2 in
            let e2_ty = type_of e2' in
              if not (e2_ty <: unref_ty) then
                raise @@ err p;
              Assign (e1',e2')
          | Ast.If (cond,thens,elses) ->
            let cond' = visit#expr cond in
              if not (is_bool (type_of cond')) then
                raise @@ err p;
              let thens' = visit#block thens in
              let elses' = visit#block elses in
                If (cond',thens',elses')
          | Ast.RangeFor (x,bty,e1,e2,blk) ->
            let bty' = visit#basic (p@>Public) bty in
              if not (is_integral bty') then
                raise @@ err p;
              let e1' = visit#expr ~lookahead_bty:bty' e1 in
              let e2' = visit#expr ~lookahead_bty:bty' e2 in
                if not (type_of e1' <: bty') then
                  raise @@ err p;
                if not (type_of e2' <: bty') then
                  raise @@ err p;
                _vmap <- (x,bty') :: _vmap;
                let blk' = visit#block blk in
                  RangeFor(x,bty',e1',e2',blk')
          | Ast.ArrayFor (x,bty,e,blk) ->
            let bty' = visit#basic (p@>Public) bty in
            let e' = visit#expr e in
            let el_ty =
              match (type_of e').data with
                | Arr (el_ty,_,_) -> el_ty
                | _ -> raise @@ err p in
              if not (el_ty <: bty') then
                raise @@ err p;
              _vmap <- (x,bty') :: _vmap;
              let blk' = visit#block blk in
                ArrayFor(x,bty',e',blk')
          | Ast.Return e ->
            _cur_rt >>=
            (fun fn_rt ->
               let e' = visit#expr ~lookahead_bty:fn_rt e in
                 if not (type_of e' <: fn_rt) then
                   raise @@ err p;
                 Return e')
            >!!> err p
          | Ast.VoidReturn ->
            begin
              match _cur_rt with
                | Some _ -> raise @@ err p
                | None -> ()
            end;
            VoidReturn
          | Ast.Assume e ->
            let e' = visit#expr e in
              if not (is_bool (type_of e')) then
                raise @@ err p;
              Assume e'
      in
      let stm' = _inject @ [p@>stm'] in
        _inject <- [];
        stm'

  end

let transform m =
  let visit = new typechecker in
    visit#fact_module m
