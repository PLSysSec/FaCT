exception NotImplemented

exception ParseError of string
exception SyntaxError of string
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

val ( << ) : string -> Pos.pos -> string

val err : Pos.pos -> exn

val errParseType : Pos.pos -> string -> exn
val errSyntax : Pos.pos -> exn
val errTypeError : Pos.pos -> exn
val errFlowError : Pos.pos -> exn
val errPassError : Pos.pos -> exn
val errPassErrorS : Pos.pos -> string -> string -> exn

val errVarNotDefined : Pos.pos -> string -> exn
val errFnNotDefined : Pos.pos -> string -> exn
val errFoundNotVar : Pos.pos -> string -> exn
val errFoundNotArr : Pos.pos -> string -> exn
val errFoundNotFn : Pos.pos -> string -> exn

