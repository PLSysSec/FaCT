exception NotImplemented
exception TypeError of string
exception LabelError of string
exception UnknownType of string
exception CallError of string
exception ForError of string
exception InternalCompilerError of string

exception VariableNotDefined of string
exception FunctionNotDefined of string
exception UnclassifiedError of string

exception TransformError of string

let ( << ) s p = s ^ " @ " ^ Pos.pos_string p

let err p =
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

