open Llvm
open Ast

exception NotImplemented
exception Error of string

let context = global_context ()
let the_module = create_module context "my cool compiler"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let env:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type' = double_type context
let string_type = const_stringz context

module SS = Set.Make(String);;

type proto = Prototype of string * string array

let codegen_proto = function
  | Prototype (name, args) ->
      (* Make the function type: double(double,double) etc. *)
      let doubles = Array.make (Array.length args) double_type' in
      let ft = function_type double_type' doubles in
      let f =
        match lookup_function name the_module with
        | None -> declare_function name ft the_module
        | Some f -> raise (Error "function already exists")
      in
      (* Set names for all arguments. *)
      Array.iteri (fun i a ->
        let n = args.(i) in
        set_value_name n a;
        Hashtbl.add named_values n a;
      ) (params f);
      f

let rec codegen_primitive = function
  | Number n  -> const_float double_type' n


let rec depends_on_secret s = function
  | BinOp(_,l,r) -> (depends_on_secret s l) || (depends_on_secret s r)
  | Variable v -> SS.exists (fun v' -> String.compare v v' == 0) s

let rec rewrite_secret s = function
  (* is a maximal munch algorithm the best option??? *)
  | If(BinOp(b,(Variable v),(Primitive(Number n))), (* figure out wildcards *)
       Mutate((Variable y),(BinOp(Plus,(Variable _),Primitive(Number n')))),
       Mutate((Variable _),(BinOp(Minus,(Variable _),Primitive(Number n''))))) -> print_string "yeppppppp\n"; Hashtbl.add env "x" (build_alloca double_type' "x" builder);
    let b = BinOp(b,Variable v, Primitive(Number n)) in
    let arr = Array.make 1 b in
    let dec1 = VarDec("boolified", CallExp("BOOLIFY",arr)) in
    let llv = codegen_dec s dec1 in
    let _ = Hashtbl.add env "boolified" llv in
    let then_and = BinOp(B_And,Primitive(Number n'),(Variable "boolified")) in
    let else_and = BinOp(B_And,Primitive(Number (-1.0 *. n'')),UnaryOp (B_Not,(Variable "boolified"))) in
    (*BinOp(Plus,BinOp(Plus, (Variable y), then_and),else_and)*)
    (*BinOp(Plus, (Variable y), then_and)*)
    Primitive(Number 19.0)
  | _ -> print_string "reqrite secret\n"; raise NotImplemented

and codegen s = function
  | Primitive p -> codegen_primitive p
  | Variable v -> print_string ("finding: " ^ v ^ "\n"); Hashtbl.find env v;
    let alloc_arg = build_alloca double_type' v builder in
    build_load alloc_arg v builder
  | Dec d -> print_string "vbsnerilgbnser\n"; codegen_dec s d
  | Seq (f::r) -> print_string "decvasd\n"; codegen s f; codegen s (Seq r)
  | Seq _ -> const_float double_type' 69.0
  | Mutate _ -> print_string "MUTATEEEEEEE\n"; raise NotImplemented
  | UnaryOp(B_Not,expr) -> print_string "UNARY\n";
    const_neg(codegen s expr)
  | CallExp (name,args) ->
    let callee =
      match lookup_function name the_module with
      | Some callee -> callee
      | None -> raise (Error ("Unknown function call:\t" ^ name))
    in
    let params = params callee in
    if Array.length params == Array.length args then () else
      raise (Error "Arity mismatch");
    let args' = Array.map (codegen s) args in
    build_call callee args' "calltmp" builder
  | BinOp(_,l, r) ->
    let lhs = codegen s l in
    let rhs = codegen s r in
    build_add lhs rhs "addtmp" builder
  | If(c,t,e) -> print_string "meh\n";
    let all_secret = true(*depends_on_secret s c (* TODO: should use andmap... *)
                  && depends_on_secret s t
                  && depends_on_secret s e*) in
        if all_secret then codegen s (rewrite_secret s (If(c,t,e))) else raise NotImplemented


(*
let rec codegen' = function
  | Seq [] -> ()
  | Seq (f::r) -> codegen f; codegen' (Seq r)
  | _ -> print_string "mvajerig"; raise NotImplemented
*)

and codegen_dec s = function
  | VarDec (name,expr) -> print_string (name ^ "\n");
    let llv = build_alloca double_type' name builder in
    let _ = build_store (codegen s expr) llv in
    Hashtbl.add env name llv; llv
  | FunctionDec (name, params, body) -> print_string "FUNCTION\n";
    let args = Array.make (List.length params) "" in
    let param_setter =
      (fun index ({ name=n }) ->
        let llv = build_alloca double_type' n builder in
        let _ = Hashtbl.add env n llv; build_store (codegen s (Variable n)) in
        Array.set args index n; (index + 1)) in
    let _ = List.fold_left param_setter 0 params in
    let the_function = codegen_proto (Prototype (name, args)) in
    let basic_block = append_block context "entry" the_function in
    position_at_end basic_block builder;
    try
      let ret_val = codegen s body in
      (*let mv = build_alloca double_type' "myvariableeee" builder in
      let _ = build_store (const_float double_type' 19.0) mv builder in*)
      let _ = build_ret ret_val builder in
      Llvm_analysis.assert_valid_function the_function;
      the_function
    with e ->
      delete_function the_function;
      raise e
    ()
