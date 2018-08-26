open Util
open Pos
open Err
open Tast
open Tast_util
open Pseudocode

let has_pub_mut params =
  let rec is_pub_mut bty =
    match bty.data with
      | Ref (bty',{data=W|RW}) -> (label_of bty').data = Public
      | Arr (bty',_,_) -> is_pub_mut bty'
      | Struct _ -> raise @@ cerr fake_pos "typecheck:has_pub_mut : Struct??"
      | _ -> false
  in
  let is_param_pub_mut {data=Param(_,bty)} = is_pub_mut bty in
    List.exists is_param_pub_mut params

let get p = function
  | Some x -> x
  | None -> raise @@ err p

let expr_fix p bty e =
  let e_bty = type_of e in
  let e' =
    if (is_integral bty) && (is_integral e_bty) then
      let bty_s = bitsize bty in
      let e_s = bitsize e_bty in
        if e_s <> bty_s then
          let upcast =
            make_ast p
              (match bty.data with
                | UInt (_,l) -> UInt (bty_s,l)
                | Int (_,l) -> Int (bty_s,l))
          in
            (p@>Cast (upcast, e), upcast)
        else e
    else e in
    if ((label_of bty).data = Secret) && ((label_of e_bty).data = Public) then
      (p@>Classify e', classify (type_of e'))
    else e'

let findvar vmap x =
  match Core.List.Assoc.find vmap x ~equal:vequal with
    | Some bty -> bty
    | None -> raise @@ cerr x.pos
                         "variable not defined: '%s'"
                         x.data

class typechecker =
  object (visit)
    val mutable _vmap : (var_name * base_type) list = []
    val mutable _fmap : (fun_name * (ret_type * params)) list = []
    val mutable _cur_fn : fun_name = fake_pos @> ""
    val mutable _cur_rt : ret_type = None
    val mutable _inject : simple_statement' pos_ast list = []
    val mutable _rp : label ref = ref (fake_pos @> Public)
    val mutable _everhis : fun_name' list = []
    val mutable _stdlibfns : function_dec list = []

    method fact_module m =
      let Ast.Module(sdecs,fdecs) = m in
      let sdecs' = List.map visit#sdec sdecs in
        (* pre-scan function defs for out-of-order definitions *)
        List.iter
          (fun fdec ->
             match fdec.data with
               | Ast.FunDec(fn,_,rt,params,_)
               | Ast.CExtern(fn,_,rt,params) ->
                 let rt' = rt >>= visit#bty %> return in
                   begin
                     match rt' with
                       | Some {data=(Bool _ | UInt _ | Int _)}
                       | None -> ()
                       | _ -> raise @@ err fdec.pos
                   end;
                   (* rev so that array lengths get added to vmap before their corresponding arrays need their type visited *)
                   let params' = List.rev_map visit#param (List.rev params) in
                     _fmap <- (fn,(rt',params')) :: _fmap)
          fdecs;
        let fdecs' = (List.map visit#fdec fdecs) in
        let fdecs' = fdecs' @ _stdlibfns in
        let minfo_fmap =
          List.map
            (function
              | {data=FunDec(fn,_,_,_,_)
                    | CExtern(fn,_,_,_)} as fdec -> (fn, fdec)
              | {data=StdLibFn(code,_,_,_)} as fdec ->
                let fn = Stdlib.name_of code in
                  (fn, fdec))
            fdecs' in
          Module(sdecs',fdecs',{ fmap=minfo_fmap })

    method sdec =
      wrap @@ fun p -> function
        | Ast.StructDef (name,fields) ->
          StructDef (name,List.map visit#field fields)

    method inline = function
      | Ast.Default -> Default
      | Ast.Always  -> Always
      | Ast.Never   -> Never

    method fntype fn Ast.{ export; inline } =
      let everhi = List.mem fn.data _everhis in
        { export; inline=visit#inline inline; everhi }

    method cfntype fn Ast.{ benign } =
      { benign }

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
    method bty ?lookahead_lexpr {pos=p; data} : Tast.base_type =
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
      wrap @@ fun p -> fun fdec ->
        _rp <- ref (fake_pos @> Public);
        match fdec with
          | Ast.FunDec(fn,ft,rt,params,body) ->
            _cur_fn <- fn;
            let ft' = visit#fntype fn ft in
            let rt' = rt >>= visit#bty %> return in
            let params' = List.map visit#param params in
              if List.mem fn.data _everhis then
                begin
                  if has_pub_mut params' then
                    raise @@ cerr p "function '%s' with public mut params is called from secret control flow" fn.data
                end;
              _cur_rt <- rt';
              let pc = fake_pos @> Public in
              let body' = visit#block p pc body in
              let body' =
                match (ends_with body').data with
                  | Return _ | VoidReturn -> body'
                  | End when rt = None -> replace_final_next body' (p@>VoidReturn)
                  | _ -> raise @@ err p
              in
                FunDec(fn,ft',rt',params',body')
          | Ast.CExtern (fn,ft,rt,params) ->
            _cur_fn <- fn;
            let ft' = visit#cfntype fn ft in
            if List.mem fn.data _everhis && not ft'.benign then
              raise @@ cerr p "calling function '%s' from secret control flow" fn.data;
            let params' = List.map visit#param params in
              CExtern(fn,ft',rt >>= visit#bty %> return,params')

    method param =
      wrap @@ fun p -> function
        | Ast.Param(x,bty) ->
          let lookahead_lexpr =
            match bty.data with
              | Ast.Arr (_,{data=Ast.LExpression {data=Ast.Variable x_len}},_) ->
                Some (p @> LDynamic x_len)
              | _ -> None
          in
          let bty' = visit#bty ?lookahead_lexpr bty in
            _vmap <- (x,bty') :: _vmap;
            Param(x,bty')

    method return pc retstm =
      let p = retstm.pos in
      let stmlbl =
        if List.mem _cur_fn.data _everhis
        then fake_pos @> Secret
        else pc +$ !_rp in
      let retnext =
        match retstm.data with
          | Ast.Return e ->
            begin
              _cur_rt >>= fun fn_rt ->
              let e' = visit#expr ~lookahead_bty:fn_rt e in
              let r_lbl = label_of fn_rt in
                if not (stmlbl <$ r_lbl) then
                  raise @@ err p;
                let e' =
                  if type_of e' <: fn_rt then
                    expr_fix p fn_rt e'
                  else
                    raise @@ err p in
                  _rp := !_rp +$ pc;
                  return @@ Return e'
            end >!!> err p
          | Ast.VoidReturn ->
            begin
              match _cur_rt with
                | Some _ -> raise @@ err p
                | None -> ()
            end;
            _rp := !_rp +$ pc;
            VoidReturn
          | _ -> raise @@ err p
      in
        p @> retnext

    method controlflow pc stm =
      let p = stm.pos in
        match stm.data with
          | Ast.If (cond,thens,elses) ->
            let cond' = visit#expr cond in
              if not (is_bool (type_of cond')) then
                raise @@ err p;
              let newpc = pc +$ (label_of @@ type_of cond') in
              let thens' = visit#block p newpc thens in
              let elses' = visit#block p newpc elses in
                If (cond',thens',elses')
          | Ast.RangeFor (x,bty,e1,e2,blk) ->
            let bty' = visit#basic (p@>Public) bty in
              if not (is_integral bty') then
                raise @@ err p;
              let e1' = visit#expr ~lookahead_bty:bty' e1 in
              let e2' = visit#expr ~lookahead_bty:bty' e2 in
              let e1' =
                if type_of e1' <: bty' then
                  expr_fix p bty' e1'
                else
                  raise @@ err p in
              let e2' =
                if type_of e2' <: bty' then
                  expr_fix p bty' e2'
                else
                  raise @@ err p in
                _vmap <- (x,bty') :: _vmap;
                let old_rp = !_rp in
                let blk' = visit#block p pc blk in
                let blk' =
                  if old_rp.data = Public && !_rp.data = Secret then
                    (* re-run the entire block, but now with secret rp *)
                    visit#block p pc blk
                  else blk' in
                  RangeFor(x,bty',e1',e2',blk')
          | Ast.ArrayFor (x,bty,e,blk) ->
            let bty' = visit#basic (p@>Public) bty in
            let e' = visit#expr e in
            let el_ty =
              match element_type (type_of e') with
                | Some el_ty -> el_ty
                | _ -> raise @@ err p in
              if not (el_ty =: bty') then
                raise @@ err p;
              _vmap <- (x,bty') :: _vmap;
              let old_rp = !_rp in
              let blk' = visit#block p pc blk in
              let blk' =
                if old_rp.data = Public && !_rp.data = Secret then
                  (* re-run the entire block, but now with secret rp *)
                  visit#block p pc blk
                else blk' in
                ArrayFor(x,bty',e',blk')

    method block p pc stms_ =
      let block' =
        match stms_ with
          | [] -> (p@>ListOfStuff [], p@>End)
          | stm :: rest ->
            let p = stm.pos in
            let next () =
              (match rest with
                | [{data=Ast.Return _} as rtstm]
                | [{data=Ast.VoidReturn} as rtstm] -> visit#return pc rtstm
                | [] -> p@>End
                | _ -> p@>(Block (visit#block p pc rest))) in
            let inblock,next' =
              match stm.data with
                | Ast.Block blk ->
                  let blk' = visit#block p pc blk in
                  let next' = next () in
                  Scope blk', next'
                | Ast.VarDec _
                | Ast.FnCall _
                | Ast.VoidFnCall _
                | Ast.Assign _
                | Ast.Assume _ ->
                  let this = visit#stm pc stm in
                    begin
                      match (next ()).data with
                        | Block ({data=ListOfStuff nexts},following) ->
                          ListOfStuff (this @ nexts), following
                        | _ ->
                          ListOfStuff this, next ()
                    end
                | Ast.If _
                | Ast.RangeFor _
                | Ast.ArrayFor _ ->
                  let controlflow' = visit#controlflow pc stm in
                  let next' = next () in
                    controlflow', next'
                | Ast.Return _
                | Ast.VoidReturn -> (ListOfStuff [], visit#return pc stm)
            in
              p@>inblock, next'
      in
        block'

    (* lexprs will always have type UInt64 in this implementation *)
    method lexpr ?lookahead_lexpr {pos=p; data} =
      let u64 = p@>UInt (64, p@>Public) in
        make_ast p begin
          match lookahead_lexpr with
            | Some lexpr -> lexpr.data
            | None -> begin
                match data with
                  | Ast.LExpression e ->
                    let e' = visit#expr ~lookahead_bty:u64 e in
                    let e_bty = type_of e' in
                      if not (e_bty <: u64) then
                        raise @@ cerr p "cannot use index of type %s" (show_base_type e_bty);
                      begin
                        match e' with
                          | ({data=IntLiteral n},_) ->
                            if n < 0 then
                              raise @@ err p;
                            if n > max_int then
                              (* this is probably int31 or int63 max or something and not uint64 max,
                                 but it's still a reasonable limit in my opinion *)
                              raise @@ err p;
                            LIntLiteral n
                          | _ ->
                            let e' = expr_fix p u64 e' in
                            let x = p @> make_fresh "lexpr" in
                              _vmap <- (x,u64) :: _vmap;
                              let var_dec = p@>VarDec(x,u64,e') in
                                _inject <- var_dec :: _inject;
                                LDynamic x
                      end
                  | Ast.LUnspecified ->
                    let x = p @> make_fresh "unspec" in
                      _vmap <- (x,u64) :: _vmap;
                      LDynamic x
              end
        end

    method expr
             ?lookahead_bty
             ?no_unbox_ref
             ?auto_box_into_ref
             ?lookahead_mut
             e_ =
      let {data=(e',bty); pos=p} = visit#expr' lookahead_bty lookahead_mut e_ in
      let e_res = (p@>e', bty) in
      let want_ref =
        match lookahead_bty, no_unbox_ref with
          | Some ({data=Ref _}), _
          | _, Some true -> true
          | _ -> false in
      let auto_box = auto_box_into_ref >!> false in
      let mut =
        match lookahead_mut with
          | Some m -> m.data
          | None -> RW in
        match bty.data with
          | Ref (subty,{data=R|RW}) ->
            begin
              match want_ref,auto_box with
                | true,false ->
                  (* keep original ref *)
                  e_res
                | true,true ->
                  (* create a new ref *)
                  (p@>Enref (p@>Deref e_res, subty), p@>Ref (subty, p@>mut))
                | false,_ ->
                  (* transparently deref, but only for vars and arrays *)
                  begin
                    let (inner_e,_) = e_res in
                      match inner_e.data with
                        | Variable _
                        | ArrayGet _ ->
                          (p@>Deref e_res, subty)
                        | _ -> e_res
                  end
            end
          | _ ->
            if auto_box
            then (p@>Enref e_res, p@>Ref (bty, p@>mut))
            else e_res

    method expr' lookahead_bty lookahead_mut =
      wrap @@ fun p -> function
        | Ast.True -> True, make_ast p @@ Bool (make_ast p Public)
        | Ast.False -> False, make_ast p @@ Bool (make_ast p Public)
        | Ast.UntypedIntLiteral n ->
          let bty = get p lookahead_bty in
            if not @@ is_integral bty then
              raise @@ cerr p "unexpected integer literal";
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
          let bty, e1', e2' = visit#binop p op e1' e2' in
            BinOp(op,e1',e2'), bty
        | Ast.TernOp (e1,e2,e3) ->
          let (e1',e2',e3'),new_bty = visit#ternop p lookahead_bty e1 e2 e3 in
            TernOp(e1',e2',e3'), new_bty
        | Ast.Select (e1,e2,e3) ->
          let (e1',e2',e3'),new_bty = visit#ternop p lookahead_bty e1 e2 e3 in
            Select(e1',e2',e3'), new_bty
        | Ast.Declassify e ->
          let e' = visit#expr e in
          let e_bty = type_of e' in
            Declassify e', declassify e_bty
        | Ast.Enref e ->
          let e' = visit#expr ?lookahead_bty e in
          let e_bty = type_of e' in
          let mut =
            match lookahead_mut with
              | Some m -> m.data
              | None -> RW in
            Enref e', p@>Ref (e_bty, p@>mut)
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
              | Arr (bty,_,_) -> bty
              | _ -> raise @@ err p in
            ArrayGet (e', index'), el_bty
        | Ast.ArrayLit es ->
          if List.length es = 0 then
            raise @@ err p;
          let es' = List.map (visit#expr ?lookahead_bty) es in
          let btys = List.map type_of es' in
          let bty = Core.List.fold btys ~init:(List.hd btys) ~f:(+:) in
          let mut =
            match lookahead_mut with
              | Some m -> m.data
              | None -> RW in
          let a_bty = p@>Arr (p@>Ref (bty,p@>mut),p@>LIntLiteral (List.length es),default_var_attr) in
            ArrayLit es', a_bty
        | Ast.ArrayZeros lexpr ->
          let lexpr' = visit#lexpr lexpr in
          let bty = get p lookahead_bty in
            if not (is_integral bty) then
              raise @@ cerr p
                         "type %s is not integral"
                         (show_base_type bty);
            let mut =
              match lookahead_mut with
                | Some m -> m.data
                | None -> RW in
            let a_bty = p@>Arr (p@>Ref (bty,p@>mut),lexpr',default_var_attr) in
              ArrayZeros (visit#lexpr lexpr), a_bty
        | Ast.ArrayCopy e ->
          let e' = visit#expr e in
          let mut =
            match lookahead_mut with
              | Some m -> m.data
              | None -> RW in
          let e_bty =
            match type_of e' with
              | {pos=p; data=Arr ({data=Ref (el_bty,{data=R|RW})},lexpr,vattr)} ->
                p@>Arr (p@>Ref (el_bty,p@>mut), lexpr, vattr)
              | _ -> raise @@ err p in
            ArrayCopy e', e_bty
        | Ast.ArrayView (e,index,len) ->
          let e' = visit#expr e in
          let index' = visit#lexpr index in
          let len' = visit#lexpr len in
          let e_bty = type_of e' in
          let new_ty =
            match e_bty.data with
              | Arr (el_ty,_,_) -> Arr (el_ty,len',default_var_attr)
              | _ -> raise @@ cerr p
                                "expected array, instead got %s"
                                (show_base_type' e_bty.data) in
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
                    Some f_bty
                | _ -> None
            end >!!> err p
          in
            StructGet (visit#expr e,field), f_bty
        | Ast.StringLiteral _ -> raise @@ cerr p "strings are not implemented yet"
        | Ast.FnCallExpr _ -> raise @@ err p (* these should all be gone after fnextract pass *)

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

    method binop p op e1 e2 =
      let bty1 = type_of e1 in
      let bty2 = type_of e2 in
        match op with
          | Ast.Plus
          | Ast.Minus
          | Ast.Multiply
          | Ast.Divide
          | Ast.Modulo ->
            if not @@ (is_integral bty1 || is_vec bty1) then
              raise @@ err p;
            if not @@ (is_integral bty2 || is_vec bty2) then
              raise @@ err p;
            if not (bty1 <: bty2 || bty2 <: bty1) then
              raise @@ cerr p
                         "incompatible types:\n  %s\n  %s"
                         (show_base_type bty1)
                         (show_base_type bty2);
            let bty = bty1 +: bty2 in
            let fix = expr_fix p bty in
              bty, fix e1, fix e2
          | Ast.Equal
          | Ast.NEqual ->
            if not (is_integral bty1 || is_bool bty1 || is_vec bty1) then
              raise @@ err p;
            if not (is_integral bty2 || is_bool bty2 || is_vec bty2) then
              raise @@ err p;
            if not (bty1 <: bty2 || bty2 <: bty1) then
              raise @@ err p;
            let l1 = label_of bty1 in
            let l2 = label_of bty2 in
            let bty = bty1.pos@>Bool (l1 +$ l2) in
            let fix = expr_fix p (bty1 +: bty2) in
              bty, fix e1, fix e2
          | Ast.GT
          | Ast.GTE
          | Ast.LT
          | Ast.LTE ->
            if not (is_integral bty1) then
              raise @@ err p;
            if not (is_integral bty2) then
              raise @@ err p;
            if not (bty1 <: bty2 || bty2 <: bty1) then
              raise @@ err p;
            let l1 = label_of bty1 in
            let l2 = label_of bty2 in
            let fix = expr_fix p (bty1 +: bty2) in
              bty1.pos@>Bool (l1 +$ l2), fix e1, fix e2
          | Ast.LogicalAnd
          | Ast.LogicalOr ->
            if not (is_bool bty1) then
              raise @@ err p;
            if not (is_bool bty2) then
              raise @@ err p;
            bty1 +: bty2, e1, e2
          | Ast.BitwiseAnd ->
            let bty =
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (m,l2) ->
                  bty1.pos@>UInt (min n m,l1 +$ l2)
                | Int (n,l1),Int (m,l2) ->
                  bty1.pos@>Int (min n m,l1 +$ l2)
                | UVec (n,bw1,l1),UVec (m,bw2,l2)
                  when n = m && bw1 = bw2 ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err p;
            in
            let fix = expr_fix p bty in
              bty, fix e1, fix e2
          | Ast.BitwiseOr
          | Ast.BitwiseXor ->
            let bty =
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (m,l2) ->
                  bty1.pos@>UInt (max n m,l1 +$ l2)
                | Int (n,l1),Int (m,l2) ->
                  bty1.pos@>Int (max n m,l1 +$ l2)
                | UVec (n,bw1,l1),UVec (m,bw2,l2)
                  when n = m && bw1 = bw2 ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err p;
            in
            let fix = expr_fix p bty in
              bty, fix e1, fix e2
          | Ast.LeftShift
          | Ast.RightShift ->
            let bty =
              match bty1.data,bty2.data with
                | UInt (n,l1),(UInt (_,l2) | Int (_,l2)) ->
                  bty1.pos@>UInt (n,l1 +$ l2)
                | Int (n,l1),(UInt (_,l2) | Int (_,l2)) ->
                  bty1.pos@>Int (n,l1 +$ l2)
                | UVec (n,bw1,l1),(UInt (_,l2) | Int (_,l2)) ->
                  bty1.pos@>UVec (n,bw1,l1 +$ l2)
                | _ -> raise @@ err p;
            in
            let fix = expr_fix p bty in
              bty, fix e1, fix e2
          | Ast.LeftRotate
          | Ast.RightRotate ->
            let bty =
              match bty1.data,bty2.data with
                | UInt (n,l1),UInt (_,l2) ->
                  bty1.pos@>UInt (n,l1 +$ l2)
                | _ -> raise @@ err p;
            in
            let fix = expr_fix p bty in
              bty, fix e1, fix e2

    method ternop p lookahead_bty e1 e2 e3 =
      let e1' = visit#expr e1 in
      let e2',e3' =
        match Ast_util.is_untyped_int e2 with
          | Some _ ->
            let e3' = visit#expr ?lookahead_bty e3 in
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
        let fix = expr_fix p new_bty in
          (e1',fix e2',fix e3'), new_bty

    method _fncall p stmlbl expected_rt fn args =
      let fnfound = findfn_opt _fmap fn in
        match fnfound with
          | None ->
            if Stdlib.contains fn then
              let fdec',args' = Stdlib.interface_of (visit#expr) p stmlbl fn args in
              let StdLibFn (code,_,rt',params') = fdec'.data in
              let fn' = Stdlib.name_of code in
                begin
                  match findfn_opt _fmap fn' with
                    | None ->
                      _stdlibfns <- fdec' :: _stdlibfns;
                      _fmap <- (fn',(rt',params')) :: _fmap
                    | _ -> ()
                end;
                visit#_fncall p stmlbl expected_rt fn' args'
            else raise @@ cerr p
                            "function not defined: '%s'"
                            fn.data
          | Some (rt,params) ->
            if (List.length args <> List.length params) then
              raise @@ cerr p
                         "expected %d param(s), got %d"
                         (List.length params)
                         (List.length args);
            let args' =
              List.map2
                (fun arg param ->
                   let Param (_,param_ty) = param.data in
                     visit#expr ~lookahead_bty:param_ty arg)
                args params in
            let args' =
              List.map2
                (fun arg param ->
                   let arg_ty = type_of arg in
                   let Param (_,param_ty) = param.data in
                     if passable_to param_ty arg_ty then
                       expr_fix p param_ty arg
                     else
                       raise @@ cerr p
                                  "argument type mismatch when calling '%s':\n\texpected %s, got %s"
                                  fn.data
                                  (ps#bty param_ty)
                                  (ps#bty arg_ty))
                args' params in
            let rt_needs_fixing =
              match expected_rt,rt with
                | Some {data=Ref(ert,mut)},Some rt ->
                  if (rt <: ert) then
                    let fresh = p@> make_fresh fn.data in
                      _inject <- (p@>FnCall (fresh,rt,fn,args')) :: _inject;
                      let e_res = expr_fix p ert (p@>Variable fresh,rt) in
                        Some (p@>Enref e_res, p@>Ref (ert, mut))
                  else
                    raise @@ cerr p
                               "expected %s got %s"
                               (ps#bty ert)
                               (ps#bty rt)
                | Some ert,Some rt ->
                  if (rt =: ert) then
                    None
                  else if (rt <: ert) then
                    let fresh = p@> make_fresh fn.data in
                      _inject <- (p@>FnCall (fresh,rt,fn,args')) :: _inject;
                      Some (expr_fix p ert (p@>Variable fresh,rt))
                  else
                    raise @@ cerr p
                               "expected %s got %s"
                               (ps#bty ert)
                               (ps#bty rt)
                | None,None -> None
                | _ -> raise @@ err p
            in
              fn, args', rt_needs_fixing

    method stm pc stm_ =
      let p = stm_.pos in
      let stmlbl =
        if List.mem _cur_fn.data _everhis
        then fake_pos @> Secret
        else pc +$ !_rp in
      let stm' =
        match stm_.data with
          | Ast.Block _ -> raise @@ err p
          | Ast.VarDec (x,bty,e) ->
            let lookahead_mut = Ast_util.get_mut bty >>| visit#mut in
            let e',e_bty,bty' =
              match Ast_util.is_unspec_arr bty with
                | Some pre_bty ->
                  let pre_bty' = visit#bty pre_bty in
                  let e' = visit#expr ~lookahead_bty:pre_bty' ?lookahead_mut e in
                  let e_bty = type_of e' in
                    begin
                      match e_bty.data with
                        | Arr (_,lexpr,_) ->
                          let bty' = visit#bty ~lookahead_lexpr:lexpr bty in
                            e',e_bty,bty'
                        | _ ->
                          raise @@ err p
                    end
                | None ->
                  let bty' = visit#bty bty in
                  let lookahead_bty = element_type bty' >!> bty' in
                  let e' = visit#expr
                             ~lookahead_bty
                             ?lookahead_mut
                             ~no_unbox_ref:(is_ref bty')
                             ~auto_box_into_ref:(is_ref bty')
                             e in
                  let e_bty = type_of e' in
                    e',e_bty,bty'
            in
            let e_fixed =
              if (e_bty <: bty') then
                expr_fix p bty' e'
              else
                raise @@ cerr p
                           "expected %s, got %s"
                           (ps#bty bty')
                           (ps#bty e_bty) in
              _vmap <- (x,bty') :: _vmap;
              VarDec (x,bty',e_fixed)
          | Ast.FnCall (x,bty,fn,args) ->
            if stmlbl.data = Secret then
              _everhis <- fn.data :: _everhis;
            let bty' = visit#bty bty in
            let fn',args',rt_needed_fixing = visit#_fncall p stmlbl (Some bty') fn args in
              _vmap <- (x,bty') :: _vmap;
              begin
                match rt_needed_fixing with
                  | Some var ->
                    VarDec (x,bty',var)
                  | None ->
                    FnCall (x,bty',fn',args')
              end
          | Ast.VoidFnCall (fn,args) ->
            if stmlbl.data = Secret then
              _everhis <- fn.data :: _everhis;
            let fn',args',_ = visit#_fncall p stmlbl None fn args in
              VoidFnCall (fn',args')
          | Ast.Assign (e1,e2) ->
            let e1' = visit#expr ~no_unbox_ref:true e1 in
            let e1_ty = type_of e1' in
            let unref_ty =
              match e1_ty.data with
                | Ref (bty,{data=W|RW}) -> bty
                | _ -> raise @@ cerr p
                                  "expected Ref@W, got %s"
                                  (show_base_type e1_ty)
            in
            let e2' = visit#expr ~lookahead_bty:unref_ty e2 in
            let e2_ty = type_of e2' in
              if e2_ty <: unref_ty then
                Assign (e1',expr_fix p unref_ty e2')
              else
                raise @@ cerr p
                           "expected %s, got %s"
                           (show_base_type unref_ty)
                           (show_base_type e2_ty)
          | Ast.If _ -> raise @@ err p
          | Ast.RangeFor _ -> raise @@ err p
          | Ast.ArrayFor _ -> raise @@ err p
          | Ast.Return _ -> raise @@ err p
          | Ast.VoidReturn -> raise @@ err p
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
