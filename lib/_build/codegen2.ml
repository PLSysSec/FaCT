open Llvm
open Ast

exception NotImplemented of string
exception Error of string

let context = global_context ()
let the_module = create_module context "ConstantC codegen"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let rec codegen_prim = function
  | Number n -> const_float double_type n

and codegen_dec = function
  | FunctionDec(name, args, body) ->
    let doubles = Array.make (List.length args) double_type in
    let ft = function_type double_type doubles in
    let the_function =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module
      | Some f -> raise (Error ("Function already defined:\t" ^ name)) in
    let args_array = Array.of_list args in
    Array.iteri (fun i a ->
      let n = args_array.(i) in
      set_value_name n a;
      Hashtbl.add named_values n a) (params the_function);
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    let ret_val = codegen_expr body in
    let _ = build_ret ret_val builder in
    Llvm_analysis.assert_valid_function the_function;
    the_function
  | VarDec _ as v -> raise (NotImplemented "Var decs")

and codegen_expr = function
  | Primitive p -> codegen_prim p
  | Variable name ->
    (try Hashtbl.find named_values name with
      | Not_found -> raise (Error ("Unknown variable:\t" ^ name)))
  | BinOp(op,lhs,rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin
      match op with
      | Plus -> build_fadd lhs_val rhs_val "addtmp" builder
      | Minus -> build_fsub lhs_val rhs_val "subtmp" builder
      | GT -> let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
              build_uitofp i double_type "booltmp" builder
      | _ -> raise (Error "Unsupported binary operator")
    end
  | UnaryOp _ as u -> raise (NotImplemented "Unary Ops")
  | If _ as i -> raise (NotImplemented "if statement")
  | Mutate _ as m -> raise (NotImplemented "Mutation")
  | Dec d -> codegen_dec d
  | Seq(e,e') -> codegen_expr e; codegen_expr e'
  | CallExp(callee, args) ->
    let callee' =
      match lookup_function callee the_module with
      | Some callee -> callee
      | None -> raise (Error("Unknown function referenced:\t" ^ callee)) in
      let params = params callee' in
      if Array.length params == List.length args then () else
        raise (Error("Arity mismatch for `" ^ callee ^ "`"));
      let args_array = Array.of_list args in
      let args' = Array.map codegen_expr (Array.of_list args) in
      build_call callee' args' "calltmp" builder
