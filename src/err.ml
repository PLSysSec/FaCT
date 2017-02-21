
let ( << ) s p = s ^ " @ " ^ Pos.pos_string p

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
  | UnknownFunction
  | ArrayNotDefined
  | PromotedTypeNotSupported
  | StoreArgsError
  | ArrayAsRefError
  | VariableAsExpression
  | AssignmentError
  | FunctionAlreadyDefined
  | RedefiningVar
  | UpdateLabelError
  | UnknownLabelError
  | TransformError
  | ArrayRequiredError
  | RedefiningFunction
  | ArityError
[@@deriving show]

exception ConstancError of error

let raise_error p e = raise (ConstancError e)

let raise_error_np e = raise (ConstancError e)

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
  | UnknownFunction -> -10
  | ArrayNotDefined -> 11
  | PromotedTypeNotSupported -> 12
  | StoreArgsError -> -13
  | ArrayAsRefError -> -14
  | VariableAsExpression -> -15
  | AssignmentError -> -16
  | FunctionAlreadyDefined -> -17
  | RedefiningVar -> 18
  | UpdateLabelError -> 19
  | UnknownLabelError -> 20
  | TransformError -> 21
  | ArrayRequiredError -> 22
  | RedefiningFunction -> 23
  | ArityError -> -24