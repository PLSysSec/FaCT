open Lexing

type pos = { file:string; line:int; lpos:int; rpos:int }
[@@deriving show]

let fake_pos = { file=""; line=0; lpos=0; rpos=0 }

type 'a pos_ast = { pos:pos; data:'a }
[@@deriving show]

let pp_pos_ast pp_data fmt { data } = pp_data fmt data

let to_pos ?buf:(b=None)
    { pos_fname=f; pos_lnum=l; pos_bol=lbl; pos_cnum=lc }
    { pos_cnum=rc } =
  match b with
  | None -> { file=f; line=l; lpos=lc-lbl+1; rpos=rc-lbl }
  | Some lb ->
    let start = (lexeme_start lb) - lb.lex_curr_p.pos_bol in
    let ends = (lexeme_end lb) - lb.lex_curr_p.pos_bol in
    { file=f; line=l; lpos=start+1; rpos=ends+1 }

let make_pos startpos endpos data = { pos=(to_pos startpos endpos); data=data }
let make_ast pos data = { pos=pos; data=data }

let pos_string { file=f; line=l; lpos=lp; rpos=rp } =
  f ^ ":" ^ string_of_int(l) ^
  ":" ^ string_of_int(lp) ^ "-" ^ string_of_int(rp)

let unpack fn {data} = fn data
let posmap fn = fun pa -> { pa with data=(fn pa.data) }

let wrap f pa = { pa with data=f pa.pos pa.data }
let xwrap f pa = f pa.pos pa.data

let rebind f pa = { pa with data=f pa }

let ( @> ) p ast = make_ast p ast
let ( << ) s p = pos_string p ^ ": " ^ s
