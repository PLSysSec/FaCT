(*===----------------------------------------------------------------------===
 * Abstract Syntax Tree (aka Parse Tree)
 *===----------------------------------------------------------------------===*)
type param = { name: string }

type expr =
  | Primitive of primitive
  | Variable of string
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
  | FunctionDec of string * string list * expr
  | VarDec of string * expr (* varname * value *)

and primitive =
  | Number of float

let _ = ()
