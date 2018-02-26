open Llvm
open Llvm.Opcode

open Llvm_ipo
open Llvm_scalar_opts
open Llvm_vectorize

type label =
  | Public
  | Secret
  | Declassify
  | Unknown

type opt_pass_name = string
[@@deriving show]

type description = string
[@@deriving show]

type details = string
[@@deriving show]

type reason = opt_pass_name * description * details
[@@deriving show]

exception VerifyError of reason

type checkerstatus =
  | Error of reason
[@@deriving show]

type state =
  | Secure
  | InSecure
  | Unchanged
  | Unknown
[@@deriving show]

let join l1 l2 =
  match l1, l2 with
    | Secret, _  -> Secret
    | _, Secret  -> Secret
    | Unknown, _ -> Unknown
    | _, Unknown -> Unknown
    | _, _       -> Public

let rec label_inference pass instr =
  let iter_operands () =
    let num_operands = Llvm.num_operands instr in
    let rec collect_operands remaining acc =
      match remaining with
        | 0 -> acc
        | n -> 
          let index = remaining - 1 in
          collect_operands index ((operand instr index)::acc)
      in
    let label = List.fold_left
      (fun acc instr' -> join acc (label_inference pass instr'))
      Public
      (collect_operands num_operands [])
      in
    
    label in
  
  match classify_value instr with
    | ValueKind.Instruction PHI -> Public
    | _ ->
  match label_of_instr pass instr with
    | Public -> Public
    | Secret -> Secret
    | Declassify -> Declassify
    | Unknown ->
      let label = iter_operands () in
      label

and name_of_instr instr =
  match instr_opcode instr with
    | Store -> value_name (operand instr 1)
    | _ -> value_name instr

and set_name_of_instr instr =
  match instr_opcode instr with
    | Store -> value_name (operand instr 1)
    | _ -> value_name instr

and label_of_instr pass instr =
  let name = value_name instr in
  let prefix = ref None in
  let set_prefix p l =
    match Core.String.is_prefix name ~prefix:p with
      | true -> prefix := Some l
      | false -> () in
  set_prefix "_secret_" Secret;
  set_prefix "_public_" Public;
  set_prefix "_unknown_" Unknown;
  set_prefix "_declassified_" Declassify;
  match !prefix with
    | None -> Unknown
    | Some l -> l

let label_module errors pass () instr =
  let instr = match instr_opcode instr, value_name instr with
    | Call,""
    | Store,_
    | Br, _
    | Ret, _ -> None
    | s  -> Some instr in
  let label instr =
    let inferred = label_inference pass instr in
    let name = name_of_instr instr in
    let prefix = match inferred with
      | Secret -> "_secret_"
      | Public -> "_public_"
      | Unknown -> raise (VerifyError(pass, "Cannot label instruction!", name)) in
    set_value_name (prefix ^ name) instr in
  match instr with
    | None -> ()
    | Some instr ->
      match label_of_instr pass instr with
        | Unknown -> label instr
        | _ -> ()

let add_error errors (Error(pass,des,det)) =
  match Hashtbl.mem errors (des,det) with
    | true -> ()
    | false -> Hashtbl.add errors (des,det) pass


let checker errors pass () instr =
  let has_secret_operand instr =
    let num_operands = Llvm.num_operands instr in
    let is_secret_operand instr =
      match label_inference pass instr with
        | Secret -> true
        | Unknown
        | Declassify
        | Public -> false in
    let rec check_operands remaining instr acc =
      match remaining with
        | 0 -> acc
        | n -> check_operands (remaining - 1) instr (acc || (is_secret_operand (operand instr (remaining - 1)))) in
    check_operands num_operands instr false in

  (* This checks that each br instruction is done on a public value *)
  let analyze_branch instr =
    let check_branch = function
      | Secret ->
        let msg = "Secret branch found on instruction" in
        let instr_str = string_of_llvalue instr in
        add_error errors (Error(pass, msg, instr_str))
      | Public -> ()
      | Unknown ->
        let msg = "Found an unknown conditional" in
        let instr_str = string_of_llvalue instr in
        add_error errors (Error(pass, msg, instr_str)) in
    match get_branch instr with
      | Some `Conditional(instr,_,_) ->
        if (is_constant instr)
          then ()
          else (check_branch (label_inference pass instr)) |> ignore
      | Some `Unconditional(_) -> ()
      | None -> () in

  (* This checks that public reads are not on secret data *)
  let analyze_read instr =
    match label_inference pass instr with
      | Secret -> ()
      | Declassify -> ()
      | Public ->
        begin
        match has_secret_operand instr with
          | true ->
            let msg = "Found Public->Secret read!" in
            Log.debug "%s" msg;
            let instr_str = string_of_llvalue instr in
            add_error errors (Error(pass,msg, instr_str))
          | false -> ()
        end
      | Unknown -> () in

  (* This checks that secret writes are not on public data *)
  let analyze_write instr =
    let value = operand instr 0 in
    let pointer = operand instr 1 in
    let value_label = label_inference pass value in
    let pointer_label = label_inference pass pointer in
    match value_label,pointer_label with
      | Secret, Public ->
        let msg = "Found Secret->Public write!" in
        Log.debug "%s" msg;
        let instr_str = string_of_llvalue instr in
        add_error errors (Error(pass,msg,instr_str))
      | Secret, Secret -> ()
      | Secret, Unknown -> ()
      | Public, Public -> ()
      | Public, Secret -> ()
      | Public, Unknown -> ()
      | Declassify, _ -> ()
      | _, Declassify -> ()
      | Unknown, Public -> ()
      | Unknown, Secret -> ()
      | Unknown, Unknown -> () in

  match instr_opcode instr with
    (* Cant check this group. The control flow instructions are done by
       the other checker anyway. They should actually be combined into one
       checker though now that I know this type exists.. *)
    | Invalid
    | Alloca
    | Ret
    | Switch
    | IndirectBr
    | Invoke
    | Invalid2
    | Call
    | PHI (* TODO: check this out again *)
    | GetElementPtr (* TODO: check this out again *)
    | Unreachable -> ()
    (* This group is all "read" operations. None of the operands can be
       secret when the LHS is public *)
    | Add
    | FAdd
    | Sub
    | FSub
    | Mul
    | FMul
    | UDiv
    | SDiv
    | FDiv
    | URem
    | SRem
    | FRem
    | Shl
    | LShr
    | AShr
    | And
    | Or
    | Xor
    | Trunc
    | ZExt
    | SExt
    | FPToUI
    | FPToSI
    | UIToFP
    | SIToFP
    | FPTrunc
    | FPExt
    | PtrToInt
    | IntToPtr
    | BitCast
    | ICmp
    | FCmp
    | UserOp1
    | UserOp2
    | VAArg
    | ExtractElement
    | InsertElement
    | ShuffleVector
    | ExtractValue
    | InsertValue
    | Fence
    | AtomicCmpXchg
    | AtomicRMW
    | Resume
    | Select
    | LandingPad -> analyze_read instr
    | Load -> () (* This needs a special checker *)
    | Store -> analyze_write instr
    | Br -> analyze_branch instr

let verify errors name llmod =
  let get_functions funs llval = llval :: funs in
  let get_blocks blocks llval = llval :: blocks in
  let funs = fold_left_functions get_functions [] llmod in
  let blocks = Hashtbl.create 100 in
  List.map
    (fun f ->
      let bbs = fold_left_blocks get_blocks [] f in
      Hashtbl.add blocks f bbs)
    funs |> ignore;

  (* Allows iteration over all instructions in the module *)
  let check_runner checker =
    let iter f bb =  List.map (fold_left_instrs checker ()) bb |> ignore in
    Hashtbl.iter iter blocks in

  (* Label everything before running the optimization *)
  check_runner (label_module errors name);

  (* We need to keep a copy of the old module in order to get a diff. TODO *)
  let llmod' = Llvm_transform_utils.clone_module llmod in
  
  (* Run the checkers *)
  check_runner (checker errors name);

  match Hashtbl.length errors with
    | 0 -> Secure
    | n -> Log.error "Hashtable length %d" n; InSecure
