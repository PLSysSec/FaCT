
exception PositionNotConfigured

type pos = { line:int; lpos:int; rpos:int; filename:string }

let cur_pos = ref None

let set_pos f =
  cur_pos := Some { line=1; lpos=0; rpos=0; filename=f }

let update_pos t =
  (match !cur_pos with
    | None -> raise PositionNotConfigured
    | Some { line=l; lpos=lp; rpos=rp; filename=f } ->
      (match t with
        | "\n" -> cur_pos := Some { line=(l+1); lpos=lp; rpos=rp; filename=f }
        | _ ->
          let len = String.length t in
          cur_pos := Some { line=l; lpos=rp; rpos=(rp+len); filename=f }))
