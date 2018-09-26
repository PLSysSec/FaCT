open Lwt_process
open Lwt

type cost =
  | Known of int
  | Unknown

type instr_cost =
  | Noop
  | Op of int

let parse_line l =
  let analyze words =
    let word_arr = Array.of_list words in
    match Array.get word_arr 2 with
      | "Unknown" -> Noop
      | _ -> match Array.get word_arr 7 with
        | n -> Op (int_of_string n) in
  match Core.String.is_prefix l "Cost Model:" with
    | false -> Noop
    | true -> analyze (Core.String.split l ' ')

let generate_cost llmod =
  let out_file = "temp.ll" in
  Llvm.print_module out_file llmod;
  let command = "opt", [|"opt"; "-S"; "-cost-model"; "-analyze"; out_file|] in
  let p = Lwt_process.pread_lines command in
  let stream = Lwt_stream.to_list p in
  let handler s =
    let costs = List.map parse_line s in
    let apply acc = function
      | Noop -> acc
      | Op c -> acc + c in
    let cost = List.fold_left apply 0 costs in
    Lwt.return cost in
  match Lwt_main.run (stream >>= handler) with
    | cost -> Sys.remove out_file; Known cost
