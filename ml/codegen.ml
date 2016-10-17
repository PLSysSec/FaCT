open Llvm
open Ast

exception NotImplemented of string
exception Error of string

let context = global_context ()
let the_module = create_module context "ConstantC codegen"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let int_type = i32_type context

let rec codegen_prim = function
  | Number n -> const_int int_type n
  | Bool b -> raise (NotImplemented "Booleans arent implemented yet")

and codegen_dec = function
  | FunctionDec(name, args, body) ->
    let doubles = Array.make (List.length args) int_type in
    let ft = function_type int_type doubles in
    let the_function =
      match lookup_function name the_module with
      | None -> declare_function name ft the_module
      | Some f -> raise (Error ("Function already defined:\t" ^ name)) in
    let args_array = Array.of_list args in
    Array.iteri (fun i a ->
        let { name=n; ty=_ } = args_array.(i) in
      set_value_name n a;
      Hashtbl.add named_values n a) (params the_function);
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    let ret_val = codegen_expr body in
    let _ = build_ret ret_val builder in
    Llvm_analysis.assert_valid_function the_function;
    the_function
  | VarDec(name,e) ->
    let e' = codegen_expr e in
    set_value_name name e';
    Hashtbl.add named_values name e';
    e'

and codegen_expr = function
  | Return e -> codegen_expr e
  | Primitive p -> codegen_prim p
  | Variable name ->
    (try Hashtbl.find named_values name with
      | Not_found -> raise (Error ("Unknown variable:\t" ^ name)))
  | BinOp(op,lhs,rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin
      match op with
      | Plus -> build_add lhs_val rhs_val "addtmp" builder
      | Minus -> build_sub lhs_val rhs_val "subtmp" builder
      | GT -> build_icmp Icmp.Ugt lhs_val rhs_val "cmptmp" builder
      | B_And -> build_and lhs_val rhs_val "andtmp" builder
    end
  | UnaryOp (B_Not,e) -> build_neg (codegen_expr e) "nottmp" builder
  | If _ -> raise (NotImplemented "if statement")
  | Mutate _ -> raise (NotImplemented "Mutation")
  | Dec d -> codegen_dec d
  | Seq(e,e') ->
    let _ = codegen_expr e in
    codegen_expr e'
  | CallExp(callee, args) ->
    let callee' =
      match lookup_function callee the_module with
      | Some callee -> callee
      | None -> raise (Error("Unknown function referenced:\t" ^ callee)) in
      let params = params callee' in
      if Array.length params == List.length args then () else
        raise (Error("Arity mismatch for `" ^ callee ^ "`"));
      let args' = Array.map codegen_expr (Array.of_list args) in
      build_call callee' args' "calltmp" builder
