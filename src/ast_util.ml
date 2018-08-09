open Pos
open Ast

let vequal x y = x.data = y.data

let is_untyped_int {data=e} =
  match e with
    | UntypedIntLiteral n -> Some n
    | _ -> None

let is_unspec_arr {data=bty} =
  match bty with
    | Arr (_,{data=LUnspecified},_) -> true
    | _ -> false
