open Command_util


let _ =
  match Sys.argv with
  | [|_;"compile";f|] -> compile f
  | [|_;"run"|] -> run ()
  | [|_;"link"|] -> link ()
  | [|_;"assemble"|] -> assemble ()
  | [|_;"share"|] -> share ()
  | [|_;"clean"|] -> clean ()
  | _ -> print_string "Unknown command\n"; ()
