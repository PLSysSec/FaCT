let return n = Some n

let bind f = function
  | Some n -> f n
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

let consume = function
  | Some () -> ()
  | None -> ()

let (%>) f g =
  Core.Fn.compose g f

let make_fresh =
  let ctr = ref 0 in
  let make_fresh' name =
    ctr := !ctr + 1;
    "__v" ^ (string_of_int !ctr) ^ "_" ^ name
  in
    make_fresh'

type 'a stack = 'a Stack.t
let push = Stack.push
let pop = Stack.pop
let top = Stack.top
let drop stack = ignore (Stack.pop stack)

type 'a mlist = 'a list ref
let mlist_mem = Core.List.Assoc.mem
let mlist_find = Core.List.Assoc.find
let mlist_push el alist =
  alist := el :: !alist
let mlist_drop alist =
  let _::tl = !alist in
    alist := tl

let force = Lazy.force
