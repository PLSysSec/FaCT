let bind f = function
  | Some n -> Some (f n)
  | None -> None

let (>>=) x f = bind f x

let (>!>) x y =
  match x with
    | Some z -> z
    | None -> y

let (>&>) n m =
  match n,m with
    | Some x, Some y -> Some (x, y)
    | _ -> None
