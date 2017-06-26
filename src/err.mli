exception NotImplemented of string

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

exception SMTSolverError of string

exception TransformError of string

val ( << ) : string -> Pos.pos -> string

val errParseType : Pos.pos -> string -> exn
val errSyntax : Pos.pos -> exn
val errTypeError : Pos.pos -> exn
val errFlowError : Pos.pos -> exn
val errPassError : Pos.pos -> exn
val errPassErrorS : Pos.pos -> string -> string -> exn

val errRedefVar : string Pos.pos_ast -> exn
val errVarNotDefined : string Pos.pos_ast -> exn
val errFnNotDefined : string Pos.pos_ast -> exn
val errFoundNotVar : string Pos.pos_ast -> exn
val errFoundNotArr : string Pos.pos_ast -> exn
val errFoundNotFn : string Pos.pos_ast -> exn

