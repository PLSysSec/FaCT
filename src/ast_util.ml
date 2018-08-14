open Pos
open Ast

let vequal x y = x.data = y.data

let is_untyped_int {data=e} =
  match e with
    | UntypedIntLiteral n -> Some n
    | _ -> None

let is_unspec_arr {data=bty} =
  match bty with
    | Arr ({data=Ref(e_bty,_)},{data=LUnspecified},_) -> Some e_bty
    | _ -> None
