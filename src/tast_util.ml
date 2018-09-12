open Pos
open Err
open Tast

let vequal x y = x.data = y.data

let expr_of (e_, bty) = e_
let type_of (e_, bty) = bty

let is_integral =
  xwrap @@ fun p -> function
    | UInt _ -> true
    | Int _ -> true
    | _ -> false

let bitsize =
  xwrap @@ fun p -> function
    | UInt (s,_)
    | Int (s,_) -> s
    | _ -> raise @@ err p

let is_signed =
  xwrap @@ fun p -> function
    | UInt _ -> false
    | Int _ -> true
    | UVec _ -> false
    | _ -> raise @@ err p

let is_bool =
  xwrap @@ fun p -> function
    | Bool _ -> true
    | _ -> false

let is_vec =
  xwrap @@ fun p -> function
    | UVec _ -> true
    | _ -> false

let is_ref =
  xwrap @@ fun p -> function
    | Ref _ -> true
    | _ -> false

let is_struct =
  xwrap @@ fun p -> function
    | Struct _ -> true
    | _ -> false

let element_type =
  xwrap @@ fun p -> function
    | Ref (bty,_) -> Some bty
    | Arr ({data=Ref (bty,_)},_,_) -> Some bty
    | UVec (s,_,l) -> Some (p@>UInt (s,l))
    | _ -> None

let rec label_of bty_ =
  let p = bty_.pos in
    match bty_.data with
      | Bool l
      | UInt (_,l)
      | Int (_,l)
      | UVec (_,_,l) -> l
      | Ref (bty,_)
      | Arr (bty,_,_) -> label_of bty
      | Struct _ -> raise @@ cerr p "???"
      | String -> p@>Public

let rec label_of_generic bty_ =
  let p = bty_.pos in
    match bty_.data with
      | Bool l
      | UInt (_,l)
      | Int (_,l)
      | UVec (_,_,l) -> Some l
      | Ref (bty,_)
      | Arr (bty,_,_) -> label_of_generic bty
      | Struct _ -> None
      | String -> Some (p@>Public)

let rec classify bty =
  let p = bty.pos in
  let sec = p@>Secret in
  let bty' =
    match bty.data with
      | Bool _ -> Bool sec
      | UInt (s,_) -> UInt (s,sec)
      | Int (s,_) -> Int (s,sec)
      | Ref (bty,m) -> Ref (classify bty,m)
      | Arr (bty,lex,vattr) -> Arr (classify bty,lex,vattr)
      | _ -> raise @@ err p
  in
    p@>bty'

let rec declassify bty =
  let p = bty.pos in
  let pub = p@>Public in
  let bty' =
    match bty.data with
      | Bool _ -> Bool pub
      | UInt (s,_) -> UInt (s,pub)
      | Int (s,_) -> Int (s,pub)
      | Ref (bty,m) -> Ref (declassify bty,m)
      | Arr (bty,lex,vattr) -> Arr (declassify bty,lex,vattr)
      | _ -> raise @@ err p
  in
    p@>bty'

let length_of =
  xwrap @@ fun p -> function
    | Arr (_,lexpr,_) -> lexpr
    | _ -> raise @@ err p

let rec ends_with_ret (_,next) =
  match next.data with
    | Block blk -> ends_with_ret blk
    | Return _
    | VoidReturn -> true
    | End -> false

let rec ends_with (_,next) =
  match next.data with
    | Block blk -> ends_with blk
    | Return _
    | VoidReturn
    | End -> next

let rec replace_final_next (blk,next) final =
  match next.data with
    | Block blk' -> (blk,next.pos @> Block (replace_final_next blk' final))
    | Return _
    | VoidReturn
    | End -> (blk,final)

let ( =$ ) l1 l2 =
  l1.data = l2.data

let ( <$ ) l1 l2 =
  match l1.data,l2.data with
    | Secret,Public -> false
    | _ -> true

let ( +$ ) l1 l2 =
  match l1.data,l2.data with
    | Public,Public -> l1.pos@>Public
    | _ -> l1.pos@>Secret

let ( =* ) m1 m2 =
  m1.data = m2.data

let ( <* ) m1 m2 =
  match m1.data,m2.data with
    | R,R -> true
    | W,W -> true
    | RW,_ -> true
    | _ -> false

let meet_mut p m1 m2 =
  match m1.data,m2.data with
    | R,W
    | W,R -> raise @@ err p
    | _,R
    | R,_ -> p@>R
    | _,W
    | W,_ -> p@>W
    | RW,RW -> p@>RW

let rec ( =: ) b1 b2 =
  match b1.data,b2.data with
    | Bool l1,Bool l2 -> true
    | UInt (n,l1),UInt (m,l2) -> n = m && l1 =$ l2
    | Int (n,l1),Int (m,l2) -> n = m && l1 =$ l2
    | Ref (rb1,m1),Ref (rb2,m2) -> rb1 =: rb2 && m1 =* m2
    | Arr (ab1,lexpr1,_),Arr (ab2,lexpr2,_) ->
      ab1 =: ab2 &&
      begin
        match lexpr1.data,lexpr2.data with
          | LIntLiteral n,LIntLiteral m -> n = m
          | LDynamic x,LDynamic y -> vequal x y
          | _ -> false
      end
    | Struct s1,Struct s2 -> s1.data = s2.data
    | UVec (n,bw1,l1),UVec (m,bw2,l2) -> n = m && bw1 = bw2 && l1 =$ l2
    | String,String -> raise @@ err b1.pos
    | _ -> false

let rec ( <: ) b1 b2 =
  match b1.data,b2.data with
    | Bool l1,Bool l2 -> l1 <$ l2
    | UInt (n,l1),UInt (m,l2) -> n <= m && l1 <$ l2
    | Int (n,l1),Int (m,l2) -> n <= m && l1 <$ l2
    | Ref (rb1,{data=R}),Ref (rb2,{data=R}) -> rb1 <: rb2
    | Ref (rb1,{data=W}),Ref (rb2,{data=W}) -> rb2 <: rb1 (* yes, this is flipped *)
    | Ref (rb1,m1),Ref (rb2,m2) -> rb1 =: rb2 && m1 <* m2
    | Arr ({data=Ref(rb1,m1)},lexpr1,_),Arr ({data=Ref(rb2,m2)},lexpr2,_) ->
      rb1 =: rb2 && m1 <* m2 &&
      begin
        match lexpr1.data,lexpr2.data with
          | LIntLiteral n,LIntLiteral m -> n = m
          | LDynamic x,LDynamic y -> vequal x y
          | _ -> false
      end
    | UVec (n,bw1,l1),UVec (m,bw2,l2) -> n = m && bw1 = bw2 && l1 <$ l2
    | _ -> false

let rec passable_to param_ty arg_ty =
  match arg_ty.data,param_ty.data with
    | Arr ({data=Ref(rb1,({data=R} as m1))},lexpr1,_),Arr ({data=Ref(rb2,m2)},lexpr2,_) ->
      rb1 <: rb2 && m1 <* m2 &&
      begin
        match lexpr1.data,lexpr2.data with
          | LIntLiteral n,LIntLiteral m -> n = m
          | _,LDynamic y -> true
          | _ -> false
      end
    | Arr ({data=Ref(rb1,m1)},lexpr1,_),Arr ({data=Ref(rb2,m2)},lexpr2,_) ->
      rb1 =: rb2 && m1 <* m2 &&
      begin
        match lexpr1.data,lexpr2.data with
          | LIntLiteral n,LIntLiteral m -> n = m
          | _,LDynamic y -> true
          | _ -> false
      end
    | _ -> arg_ty <: param_ty

let ( +: ) b1 b2 =
  match b1.data,b2.data with
    | Bool l1,Bool l2 -> b1.pos@>Bool (l1 +$ l2)
    | UInt (n,l1),UInt (m,l2) ->
      b1.pos@>UInt (max n m, l1 +$ l2)
    | Int (n,l1),Int (m,l2) ->
      b1.pos@>Int (max n m, l1 +$ l2)
    | UVec (n,bw1,l1),UVec (m,bw2,l2)
      when n = m && bw1 = bw2 ->
      b1.pos@>UVec (n, bw1, l1 +$ l2)
    | _ -> raise @@ cerr b1.pos "XXX" (*XXX*)

let findfn fmap fname =
  match Core.List.Assoc.find fmap fname ~equal:vequal with
    | Some fnty -> fnty
    | None -> raise @@ cerr fname.pos
                         "function not defined: '%s'"
                         fname.data

let findfn_opt fmap fname =
  Core.List.Assoc.find fmap fname ~equal:vequal

