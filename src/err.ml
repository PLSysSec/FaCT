open Ast

let ( << ) s p = s ^ " @ " ^ Pos.pos_string p

let pp_type_error fmt =
  let pp = Format.pp_print_text fmt in
  pp "Type error";

type error =
  | LexingError
  | SyntaxError
  | TypeError of { expected: Ast.ctype; actual: Ast.ctype }
  | TypeErrorGeneric of { expected: string; actual: Ast.ctype }
  | UnmatchedTypeError
  | TypeFlowError of { lhs: Ast.ctype; rhs: Ast.ctype }
  | LabelFlowError of { lhs: Ast.label; rhs: Ast.label }
  | KindError of { expected: Ast.kind; actual: Ast.kind }
  (*| FunctionCallArgError of Ast.ctype * Ast.ctype*)
  | VariableNotDefined of string
  | FunctionNotDefined of string
  | UnknownFunction of string
  | ArrayNotDefined of string
  
    (* TODO: This should take a Cast.ctype. cast.ml needs to be refactored
     *        to remove all environment code because it creates a circular
     *        dependency
     *)
  | PromotedTypeNotSupported
  
  | StoreArgsError
  | ArrayAsRefError
  | ArrayAsExpression
  | AssignmentError of string
  | FunctionAlreadyDefined of string
  | RedefiningVar of string
  | UpdateLabelError
  | UnknownLabelError
  | TransformError
  (*| ArrayRequiredError*)
  | RedefiningFunction of string
  | ArityError of { expected: int; actual: int }
[@@deriving show]

exception ConstancError of error * Pos.pos option

let raise_error p e = raise (ConstancError (e, (Some p)))

let raise_error_np e = raise (ConstancError (e, None))

let error_code = function
  | LexingError -> 1
  | SyntaxError -> 2
  | TypeError _ -> 3
  | TypeFlowError _ -> 4
  | VariableNotDefined _ -> 8
  | FunctionNotDefined _ -> 9
  | UnknownFunction _ -> -10
  | ArrayNotDefined _ -> 11
  | PromotedTypeNotSupported -> -12
  | StoreArgsError -> -13
  | ArrayAsRefError -> -14
  | ArrayAsExpression -> -15
  | AssignmentError _ -> -16
  | FunctionAlreadyDefined _ -> -17
  | RedefiningVar _ -> 18
  | UpdateLabelError -> 19
  | UnknownLabelError -> -20
  | TransformError -> -21
  | RedefiningFunction _ -> 23
  | ArityError _ -> -24
  | TypeErrorGeneric _ -> 25
  | UnmatchedTypeError -> -26
  | LabelFlowError _ -> 27
  | KindError _ -> 28

let build_expected_error name expected actual =
  name ^ ": expected `" ^ expected ^ "`, actual `" ^ actual ^ "`"

let build_flow_error name lhs rhs =
  name ^ ": `" ^ rhs ^ "` cannot flow to `" ^ lhs ^ "`"

let string_of_error' = function
  | LexingError -> "Lexing Error"
  | SyntaxError -> "Syntax Error"
  | TypeError { expected; actual } -> 
    build_expected_error
      "Type Error" (Ast.show_ctype expected) (Ast.show_ctype actual)
  | TypeErrorGeneric { expected; actual } ->
    build_expected_error "Type Error" expected (Ast.show_ctype actual)
  | TypeFlowError { lhs; rhs } ->
    build_flow_error "Type Flow Error" (Ast.show_ctype rhs) (Ast.show_ctype lhs)
  | LabelFlowError { lhs; rhs } ->
    build_flow_error
      "Label Flow Error" (Ast.show_label rhs) (Ast.show_label lhs)
  | KindError { expected; actual } ->
    build_expected_error
      "Type Error" (Ast.show_kind expected) (Ast.show_kind actual)
  | ArrayNotDefined name -> "Array Not Defined: `" ^ name ^ "`" 
  | RedefiningFunction name -> "Redefining Function: `" ^ name ^ "`"
  | RedefiningVar name -> "Redefining Variable: `" ^ name ^ "`"
  | FunctionNotDefined name -> "Function Not Defined: `" ^ name ^ "`"
  | VariableNotDefined name -> "Variable Not Defined: `" ^ name ^ "`"
  | FunctionAlreadyDefined name -> "Function Already Defined: `" ^ name ^ "`"
  | PromotedTypeNotSupported -> "Promoted Type Not Supported"
  | StoreArgsError -> "Store args error"
  | ArrayAsRefError -> "Array as ref error"
  | ArrayAsExpression -> "Array as expression error"
  | UnknownFunction f -> "Unknown function: `" ^ f ^ "`"
  | AssignmentError v -> "Cannot assign an array to variable `" ^ v ^ "`"
  | ArityError { expected; actual } ->
    build_expected_error
      "Arity Error" (string_of_int expected) (string_of_int actual)
  | UpdateLabelError -> "Update Label Error"
  | UnmatchedTypeError -> "Unmatched Type Error"
  | UnknownLabelError -> "Unknown Label Error"
  | TransformError -> "Transform Error"

let string_of_error e = function
  | None -> (string_of_int (error_code e)) ^ " @ Unknown..."
  | Some p -> (string_of_error' e) ^ " @ " ^ (Pos.pos_string p)