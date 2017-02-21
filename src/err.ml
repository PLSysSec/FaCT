exception ConstancError

exception NotImplemented
exception TypeError of string
exception UnknownType of string
exception CallError of string
exception ForError of string
exception InternalCompilerError of string

exception VariableNotDefined of string
exception FunctionNotDefined of string
exception UnclassifiedError of string

exception TransformError of string

let ( << ) s p = s ^ " @ " ^ Pos.pos_string p

(*let err p =
  InternalCompilerError(__LOC__ << p)
let errTypeError p =
  TypeError("Types cannot be unified for given operation" << p)
let errFlowError p =
  TypeError("Invalid type flow" << p)
let errPassError p =
  TypeError("Cannot call function with this type" << p)
let errPassErrorS p sty1 sty2 =
  TypeError("Cannot call function with these types: "^sty1^", "^sty2 << p)

let errVarNotDefined v =
  VariableNotDefined("Variable `" ^ v ^ "` not defined")
let errFnNotDefined v =
  FunctionNotDefined("Function `" ^ v ^ "` not defined")
let errFoundNotVar v =
  TypeError("Cannot use `" ^ v ^ "` as variable")
let errFoundNotArr v =
  TypeError("Cannot use `" ^ v ^ "` as array")
let errFoundNotFn v =
  TypeError("Cannot use `" ^ v ^ "` as function")
*)

let pp_type_error fmt =
  let pp = Format.pp_print_text fmt in
  pp "Type error";

type error =
  | LexingError
  | SyntaxError
  | TypeError
  | TypeFlowError
  | FunctionCallArgError of Ast.ctype * Ast.ctype
  | FunctionCallLabelError
  | FunctionCallKindError
  | VariableNotDefined
  | FunctionNotDefined
  | UnknownFunction (* Used in codegen. If this is called, there is a bug *)
  | ArrayNotDefined
  | PromotedTypeNotSupported
  | StoreArgsError (* Codegen *)
  | ArrayAsRefError (* Codegen *)
  | VariableAsExpression (* Codegen *)
  | AssignmentError (* Codegen *)
  | FunctionAlreadyDefined (* Codegene *)
  | RedefiningVar
  | UpdateLabelError
  | UnknownLabelError
  | TransformError
  | ArrayRequiredError
[@@deriving show]

let raise_error p e = raise ConstancError

let raise_error_np e = raise ConstancError

let error_code = function
  | LexingError -> 1
  | SyntaxError -> 2
  | TypeError -> 3
  | TypeFlowError -> 4
  | FunctionCallArgError _ -> 5
  | FunctionCallLabelError -> 6
  | FunctionCallKindError -> 7
  | VariableNotDefined -> 8
  | FunctionNotDefined -> 9
  | UnknownFunction -> 10
  | ArrayNotDefined -> 11
  | PromotedTypeNotSupported -> 12
  | StoreArgsError -> 13
  | ArrayAsRefError -> 14
  | VariableAsExpression -> 15
  | AssignmentError -> 16
  | FunctionAlreadyDefined -> 17
  | RedefiningVar -> 18
  | UpdateLabelError -> 19
  | UnknownLabelError -> 20
  | TransformError -> 21
  | ArrayRequiredError -> 22