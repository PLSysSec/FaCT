let bind f = function
  | Some n -> Some (f n)
  | None -> None

let (>>=) x f = bind f x

let (>!>) x y =
  match x with
    | Some z -> z
    | None -> y

let (>!!>) x exn =
  match x with
    | Some z -> z
    | None -> raise exn

let (>&>) n m =
  match n,m with
    | Some x, Some y -> Some (x, y)
    | _ -> None

let (%>) f g =
  Core.Fn.compose g f

let make_fresh =
  let ctr = ref 0 in
  let make_fresh' name =
    ctr := !ctr + 1;
    "__v" ^ (string_of_int !ctr) ^ "_" ^ name
  in
    make_fresh'
