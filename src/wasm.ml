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

let append dest to_add : unit =
  dest.contents <- dest.contents ^ to_add;

class wasm (m: Tast.fact_module) = 
  object (visit)

  method sdec ({pos=p; data}: Tast.struct_type) : unit = 
    raise @@ cerr p "unimp sdec"

  method varname {pos=p;data} = 
    "$" ^ data

  method bty {pos=p;data} = 
    match data with
    | Bool {data=Secret} -> "s32"
    | Bool {data=Public} -> "i32"
    | UInt (32, {data=Secret}) | Int (32, {data=Secret}) -> "s32"
    | UInt (32, {data=Public}) | Int (32, {data=Public}) -> "i32"
    | UInt (64, {data=Secret}) | Int (64, {data=Secret}) -> "s64"
    | UInt (64, {data=Public}) | Int (64, {data=Public}) -> "i64"
    | Ref (bty, _) -> visit#bty bty
    | _ -> raise @@ err p

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

  method fdec ({pos=p; data} as fdec: Tast.function_dec) : string = 
    match data with
      | FunDec(name, fnattr, rt, params, body) -> 
        let res = ref "" in
        append res "(func ";
        append res ("$" ^ name.data ^ " ");

        append res (visit#fsig rt params);

        let vars = collect_locals fdec in
        (* print_string (String.concat " " (List.map 
          (fun (name, bty) -> (show_var_name name) ^ (show_base_type bty) )
          vars)); *)
        append res (List.map visit#local vars |> String.concat " ");

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