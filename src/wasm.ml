open Util
open Pos
open Err
open Tast
open Llvm
open Pseudocode
open Codegen

class local_collector m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val mutable _vars : (var_name * base_type) list = []

    method _vars () =
      List.rev _vars

    (* method param param =
      begin
        match param.data with
          | Param (x,bty) ->
            _vars <- (x,bty) :: _vars
      end; super#param param 
      *)

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
          | VarDec (x,bty,_)
          | FnCall (x,bty,_,_) ->
            _vars <- (x,bty) :: _vars
          | _ -> ()
      end; super#stm stm

  end

let collect_locals fdec =
  let m = Module([],[fdec],{fmap=[]}) in
  let visit = new local_collector m in
    visit#fact_module () |> ignore;
    visit#_vars ()

let rec build_cast pos prefix oldsize newsize is_signed wasmexpr = 
  match (oldsize, newsize, is_signed) with
  (* TODO: cleanup this monstrocity *)
  (* It may not be correct either... *)
  | (8, 32, true) | (8, 64, true)
  | (16, 32, true) | (16, 64, true)
  | (32, 64, true) -> 
    Printf.sprintf "(%s%d.extend%d_s %s)" prefix newsize oldsize wasmexpr
  | (8, 64, false) | (16, 64, false) | (32, 64, false) ->
    Printf.sprintf "(%s%d.extend32_u %s)" prefix newsize wasmexpr
  | (64, 32, _) -> 
    Printf.sprintf "(%s32.wrap_%s64 %s)" prefix prefix wasmexpr
  | (64, 16, _) | (64, 8, _) -> 
    build_cast pos prefix 32 newsize is_signed 
      (build_cast pos prefix 64 32 is_signed wasmexpr) 
    (* first cast the thing from 64 to 32 bits, then cast it down again*)
  | (32, 16, _) -> 
    Printf.sprintf "(%s32.and (%s32.const 0xffff) %s)" prefix prefix wasmexpr
  | (32, 8, _) | (16, 8, _) -> 
    Printf.sprintf "(%s32.and (%s32.const 0xff) %s)" prefix prefix wasmexpr
  | (8, 32, false) | (16, 32, false) 
  | (8, 8, _) | (16, 16, _) | (8, 16, false)
  | (32, 32, _) | (64, 64, _) -> wasmexpr (* no need to cast *)
  | (8, 16, true) ->
    build_cast pos prefix 32 16 is_signed 
      (build_cast pos prefix 8 32 is_signed wasmexpr)
    (* first sextend to 32 bits then downsize to 16 *)
  | _ -> raise @@ cerr pos "unimp cast"

let append dest to_add : unit =
  dest.contents <- dest.contents ^ to_add;

class wasm (m: Tast.fact_module) = 
  object (visit)

  method sdec ({pos=p; data}: Tast.struct_type) : unit = 
    raise @@ cerr p "unimp sdec"

  method varname {pos=p;data} = 
    "$" ^ data

  method is_secret {pos=p;data} = 
    match data with
    | Bool {data=Secret} 
    | UInt (_, {data=Secret})
    | Int (_, {data=Secret}) -> true
    | Bool {data=Public} 
    | UInt (_, {data=Public})
    | Int (_, {data=Public}) -> false
    | _ -> raise @@ cerr p "unimp secret"
  
  method int_bitwidth {pos=p;data} = 
    match data with 
    | Bool _ -> 1
    | UInt (w, _) -> w
    | Int (w, _) -> w
    | _ -> raise @@ cerr p "unimp bitwidth"

  method bty {pos=p;data} = 
    match data with
    | Bool {data=Secret} -> "s32"
    | Bool {data=Public} -> "i32"
    | UInt (32, {data=Secret}) | Int (32, {data=Secret})
    | UInt (8, {data=Secret})  | Int (8, {data=Secret})
    | UInt (16, {data=Secret}) | Int (16, {data=Secret}) -> "s32"
    | UInt (32, {data=Public}) | Int (32, {data=Public})
    | UInt (8, {data=Public})  | Int (8, {data=Public})
    | UInt (16, {data=Public}) | Int (16, {data=Public}) -> "i32"
    | UInt (64, {data=Secret}) | Int (64, {data=Secret}) -> "s64"
    | UInt (64, {data=Public}) | Int (64, {data=Public}) -> "i64"
    | Ref (bty, _) -> visit#bty bty
    | _ -> raise @@ cerr p "unimp bty"

  method param {pos=p;data} = 
    match data with
      | Param (x, bty) -> 
        "(param " ^ (visit#varname x) ^ " " ^ (visit#bty bty) ^ ")"
  
  method rty rt = 
    match rt with
    | Some bty -> 
      "(result " ^ visit#bty bty ^ ")"
    | None -> ""

  method fsig rt params = 
    let params = List.map visit#param params |> String.concat " " in
    let ret = visit#rty rt in
    params ^ " " ^ ret
  
  method local l = 
    match l with
    | (name, bty) -> "(local " ^ (visit#varname name) ^ " " ^ (visit#bty bty) ^ ")"
  
  method binop p op is_signed we1 we2 bty =
    let sign = (if is_signed then "s" else "u") in
      match op with 
      | Ast.Plus -> Printf.sprintf "(%s.add %s %s)" bty we1 we2
      | Ast.Minus -> Printf.sprintf "(%s.sub %s %s)" bty we1 we2
      | Ast.Multiply -> Printf.sprintf "(%s.mul %s %s)" bty we1 we2
      | Ast.Divide -> 
        Printf.sprintf "(%s.div_%s %s %s)" bty sign we1 we2
      | Ast.Modulo -> 
        if is_signed then Printf.sprintf "(%s.rem_s %s %s)" bty we1 we2
        else Printf.sprintf "(%s.rem_u %s %s)" bty we1 we2
      | Ast.Equal -> Printf.sprintf "(%s.eq %s %s)" bty we1 we2
      | Ast.NEqual -> Printf.sprintf "(%s.ne %s %s)" bty we1 we2
      | Ast.GT -> 
        Printf.sprintf "(%s.gt_%s %s %s)" bty sign we1 we2
      | Ast.GTE -> 
        Printf.sprintf "(%s.ge_%s %s %s)" bty sign we1 we2
      | Ast.LT -> 
        Printf.sprintf "(%s.lt_%s %s %s)" bty sign we1 we2
      | Ast.LTE -> 
        Printf.sprintf "(%s.le_%s %s %s)" bty sign we1 we2
      | Ast.LogicalAnd | Ast.BitwiseAnd -> Printf.sprintf "(%s.and %s %s)" bty we1 we2
      | Ast.LogicalOr | Ast.BitwiseOr -> Printf.sprintf "(%s.or %s %s)" bty we1 we2
      | Ast.BitwiseXor -> Printf.sprintf "(%s.xor %s %s)" bty we1 we2
      | Ast.LeftShift -> Printf.sprintf "(%s.shl %s %s)" bty we1 we2
      | Ast.RightShift -> Printf.sprintf "(%s.shr_%s %s %s)" bty sign we1 we2
      | Ast.LeftRotate -> Printf.sprintf "(%s.rotl %s %s)" bty we1 we2
      | Ast.RightRotate -> Printf.sprintf "(%s.rotr %s %s)" bty we1 we2
  
  method unop p op wexpr wbty =
    match op with
    | Ast.LogicalNot -> 
      Printf.sprintf "(%s.xor %s (%s.const 1))" wbty wexpr wbty
    | Ast.BitwiseNot -> 
      Printf.sprintf "(%s.xor %s (%s.const -1))" wbty wexpr wbty
    | Ast.Neg -> 
      Printf.sprintf "(%s.sub (%s.const 0) %s)" wbty wbty wexpr

  method expr ({pos=p; data}, bty) =
    let wbty = visit#bty bty in
    match data with
      | True -> "(" ^ wbty ^ ".const 1)"
      | False -> "(" ^ wbty ^ ".const 0)"
      | IntLiteral n -> Printf.sprintf "(%s.const %d)" wbty n
      | Variable x -> Printf.sprintf "(get_local %s)" (visit#varname x)
      | Enref e -> visit#expr e
      | Deref e -> visit#expr e
      | UnOp (op, e) -> 
        visit#unop p op (visit#expr e) wbty
      | BinOp (op, e1, e2) ->
        let we1 = visit#expr e1 in
        let we2 = visit#expr e2 in
        let e1ty = Tast_util.type_of e1 in
        let is_signed = Tast_util.(not (is_bool e1ty) && is_signed e1ty) in
          visit#binop p op is_signed we1 we2 wbty
      | Classify e -> Printf.sprintf "(%s.classify %s)" wbty (visit#expr e)
      | Declassify e -> Printf.sprintf "(%s.declassify %s)" wbty (visit#expr e)
      | TernOp (cond, e1, e2) | Select (cond, e1, e2) -> 
        let wcond = visit#expr cond in
        let we1 = visit#expr e1 in
        let we2 = visit#expr e2 in
        let (_, cond_bty) = cond in
        let op = (if (visit#is_secret cond_bty) then "sselect" else "select") in
        Printf.sprintf "(%s %s %s %s)" op we1 we2 wcond
      | Cast (castty, e) -> 
        let we = visit#expr e in
        let (_, oldbty) = e in
        let oldsize = visit#int_bitwidth oldbty in
        let newsize = visit#int_bitwidth castty in
        let is_signed = Tast_util.is_signed oldbty in
        let prefix = if (visit#is_secret castty) then "s" else "i" in
        build_cast p prefix oldsize newsize is_signed we
      | _ -> raise @@ cerr p "unimp expr"

  method assignment p dest wexpr = 
    match dest with
      | ({data=Variable v}, _) -> 
        "(set_local " ^ (visit#varname v) ^ " " ^ wexpr ^ ")"
      | _ -> raise @@ cerr p "unimp assign"

  method stm {pos=p; data} =
    match data with
      | VarDec (x, bty, e) -> 
        "(set_local " ^ (visit#varname x) ^ " " ^ visit#expr e ^ ")"
      | Assign (e1, e2) ->
        visit#assignment p e1 (visit#expr e2)
      | Cmov (e1, cond, e2) -> 
        begin
          match e1 with
          | ({data=Variable v}, bty) -> 
            let (_, cond_bty) = cond in
            let op = (if (visit#is_secret cond_bty) then "sselect" else "select") in
            Printf.sprintf "(set_local %s (%s %s (get_local %s) %s))" 
            (visit#varname v) op (visit#expr e2) (visit#varname v) (visit#expr cond)
            (* If cond then select e2, otherwise select e1, 
              then move the resulting value back into e1 *)
          | _ -> raise @@ cerr p "unimp cmov"
        end
      | FnCall (x, bty, fn, args) ->
        let wargs = List.map visit#expr args |> String.concat " " in
        let fname = fn.data in
        Printf.sprintf "(set_local %s (call $%s %s))" 
          (visit#varname x) fname wargs
      | VoidFnCall (fn, args) ->
        let wargs = List.map visit#expr args |> String.concat " " in
        let fname = fn.data in
        Printf.sprintf "(call $%s %s)" fname wargs
      | Assume e -> ""

  method block ({pos=p; data}, next) = 
    let res = ref "" in
    begin
      match data with
        | Scope blk -> 
          append res (visit#block blk);
        | ListOfStuff stms -> 
          append res((List.map visit#stm stms) |> String.concat " ");
        | If (cond,thens,elses) ->
          append res (Printf.sprintf "(if %s (%s) (%s))" 
            (visit#expr cond)
            (visit#block thens)
            (visit#block elses)
          )
        | _ -> raise @@ cerr p "unimp block"
    end;
    append res (visit#next next);
    res.contents

  method next {pos=p;data} =
    match data with
      | Block blk -> 
        visit#block blk
      | End -> ""
      | Return e -> Printf.sprintf "(return %s)" (visit#expr e)
      | VoidReturn -> "(return)"

  method fdec ({pos=p; data} as fdec: Tast.function_dec) : string = 
    match data with
      | FunDec(name, fnattr, rt, params, body) -> 
        let res = ref "" in
        append res "(func ";
        append res ("$" ^ name.data ^ " ");

        append res ((visit#fsig rt params) ^ " ");

        let vars = collect_locals fdec in
        append res ((List.map visit#local vars |> String.concat " ") ^ " ");

        append res (visit#block body);

        append res ")";
        res.contents
      | _ -> raise @@ cerr p "unimp fdec"
  
  method fact_module () = 
    let res = ref "" in
    append res "(module ";
    let Module (sdecs,fdecs,minfo) = m in
    let _ = List.map visit#sdec sdecs in
    
    append res (List.map visit#fdec (List.rev fdecs) |> String.concat " ");

    append res ")";
    res.contents
end

let codegen m = 
  let visit = new wasm m in
    visit#fact_module()