open Llvm
open Ast

exception NotImplemented of string
exception Error of string

let context = global_context ()
let the_module = create_module context "ConstantC codegen"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let int_type = i32_type context
let bool_type = i1_type context
let i8_t = i8_type context

let rec codegen_prim = function
  | Number n -> const_int int_type n
  | Boolean true -> const_int bool_type 1
  | Boolean false -> const_int bool_type 0
  | ByteArray _ -> raise (NotImplemented "Byte arrays are not implemented")

and codegen_fdec = function
  | FunctionDec(n,args,ty,body) ->
    let doubles = Array.make (List.length args) int_type in
    let ft = function_type int_type doubles in
    let the_function =
      match lookup_function n the_module with
      | None -> declare_function n ft the_module
      | Some f -> raise (Error ("Function already defined:\t" ^ n)) in
    let args_array = Array.of_list args in
    Array.iteri (fun i a ->
                  let { name=n; ty=_ } = args_array.(i) in
                  set_value_name n a;
                  Hashtbl.add named_values n a)
                (params the_function);
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;
    let _ = List.map codegen_stm body in
    Llvm_analysis.assert_valid_function the_function;
    the_function

and codegen_unop op e =
  match op with
  | B_Not -> build_neg (codegen_expr e) "nottmp" builder
  | Negate -> raise (NotImplemented "Negate unary op is not implemented")

and codegen_binop op e e' =
  let lhs = codegen_expr e in
  let rhs = codegen_expr e' in
  begin
    match op with
    | Plus -> build_add lhs rhs "addtmp" builder
    | Minus -> build_sub lhs rhs "subtmp" builder
    | GT -> build_icmp Icmp.Ugt lhs rhs "cmptmp" builder
    | B_And -> build_and lhs rhs "andtmp" builder
  end


and codegen_expr = function
  | Variable v ->
    (try Hashtbl.find named_values v with
     | Not_found -> raise (Error ("Unknown variable:\t" ^ v)))
  | Unop(op,e) -> codegen_unop op e
  | Binop(op,e,e') -> codegen_binop op e e'
  | Primitive p -> codegen_prim p
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

and codegen_stm = function
  | Return e -> build_ret (codegen_expr e) builder
  | While _ -> raise (NotImplemented "Loops not implemented")
  | If _ -> raise (NotImplemented "If statements not implemented")
  | Assign(n,e) -> raise (NotImplemented "Mutation not implemented yet")
  | VarDec(n,_,e) ->
    let e' = codegen_expr e in
    set_value_name n e';
    Hashtbl.add named_values n e';
    e'

and codegen_module = function
  | FDec f -> List.map codegen_fdec f

let codegen =
  let _ = print_string "Initializing codegen...\n" in
  (*let printf_ty = var_arg_function_type int_type [| pointer_type i8_t |] in
    let printf = declare_function "printf" printf_ty the_module in*)
  codegen_module
