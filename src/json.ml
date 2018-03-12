

let json_out json f =
  (* Write to JSON file and stderr *)
  let str : string = (Yojson.Basic.to_string json) in
  let dir = (Filename.dirname f) ^ "/.fact/" in
  let json_file = dir ^ (Filename.basename f) ^ ".json" in
  (*Printf.fprintf Pervasives.stderr "%s\n" str;*)
  Core.Out_channel.write_all json_file ~data:str

let make_json json in_files s err unit_tests =
  match json, in_files, err with
    | true, (f::r), true ->
      Log.debug "Making JSON for program error";
      (* Make JSON string *)
      let ss = Str.bounded_split (Str.regexp_string " ") s 3 in
      let sss = Str.bounded_split (Str.regexp_string ":") (List.nth ss 0) 4 in
      let row = List.nth sss 1 in
      let cols = Str.bounded_split (Str.regexp_string "-") (List.nth sss 2) 2 in
      let col_start = List.nth cols 0 in
      let col_end = List.nth cols 1 in
      let msg = List.nth ss 2 in
      let msg' = String.concat " " ss in
      let json = Yojson.Basic.from_string
        ("{\"types\":{},\"status\":\"error\",\"errors\":[
            { \"message\" : \"" ^ msg' ^ "\"
            , \"start\"   : { \"column\":" ^ col_start ^ ", \"line\":" ^ row ^ "} 
            , \"stop\"    : { \"column\":" ^ col_end   ^ ", \"line\":" ^ row ^ "} 
        }]}") in
      json_out json f
   | true, (f::r), false ->
      Log.debug "Making JSON for program success";
      let json = Yojson.Basic.from_string
        ("{\"types\":{},\"status\":\"safe\",\"errors\":[],
           \"unit_test\": \"" ^ unit_tests ^ "\"
          }") in
      json_out json f
    | _ -> Log.debug "Not making JSON"
