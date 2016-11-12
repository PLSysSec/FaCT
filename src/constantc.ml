open Command_util


let _ =
  match Sys.argv with
  | [|_;"compile"|] ->  print_string "Need an AST...\n"
  | [|_;"run"|] -> run ()
  | [|_;"link"|] -> link ()
  | [|_;"assemble"|] -> assemble ()
  | [|_;"share"|] -> share ()
  | [|_;"harness"|] -> harness ()
  | [|_;"clean"|] -> clean ()
  | [|_;"all"|] -> ignore(List.map (fun c -> c ()) commands)
  | _ -> print_string "Unknown command\n"; ()
