open Lexing

type pos = { file:string; line:int; lpos:int; rpos:int }
[@@deriving show, eq]

type 'a pos_ast = { pos:pos; data:'a }
[@@deriving show, eq]

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
  "file " ^ f ^ ", line " ^ string_of_int(l) ^
  ", from " ^ string_of_int(lp) ^ "-" ^ string_of_int(rp)

let unpack fn = fun pa -> fn pa.data
let posmap fn = fun pa -> { pa with data=(fn pa.data) }
