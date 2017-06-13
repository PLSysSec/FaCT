open Pos
open Ast

let is_int' = function
  | Int _ -> true
  | UInt _ -> true
  | _ -> false
let is_int = unpack is_int
