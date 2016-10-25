(*===----------------------------------------------------------------------===
 * Abstract Syntax Tree (aka Parse Tree)
 *===----------------------------------------------------------------------===*)

type constantc_module = FDec of fdec list

and fdec = FunctionDec of string * param list * constantc_type * stm list

and constantc_type =
  | Int
  | Bool
  | String
  | NoneType

and param = { name: string; ty: constantc_type }

and stm =
  | VarDec of string * constantc_type * expr
  | Assign of string * expr
  | If of expr * stm list * stm list
  | While of expr * stm list
  | Return of expr

and expr =
  | VarExp of string
  | Unop of unop * expr
  | BinOp of binop * expr * expr
  | Primitive of primitive
  | CallExp of string * expr list

and unop =
  | B_Not
  | Negate

and binop =
  | Plus
  | Minus
  | GT
  | B_And
  | B_Or

and primitive = Number of int
              | ByteArray of string
              | Boolean of bool


let ty_to_string = function
  | Int -> "Int"
  | Bool -> "Bool"
  | String -> "String"
  | NoneType -> "NoneType"

(*********************************
type expr =
  | Primitive of primitive
  | VarExp of string
  | BinOp of op * expr * expr (* operator * left * right *)
  | UnaryOp of unop * expr (* unary operator * expr *)
  | If of expr * expr * expr (* condition * then expr * else expr *)
  | Mutate of expr * expr (* change to Assign *)
    (* mutate a variable value. ex) x += 1*) (* variable * body *)
  | Dec of dec
  | Seq of expr * expr
  | CallExp of string * expr list (* variable * args *)
  | Return of expr

and unop =
  | B_Not

and op =
  | Plus
  | Minus
  | GT
  | B_And

and dec =
  | FunctionDec of string * param list * expr
  | VarDec of string * expr (* varname * value *)

and param = { name: string; ty: constantc_type }

and primitive =
  | Number of int
  | Bool of bool

let _ = ()
*)
