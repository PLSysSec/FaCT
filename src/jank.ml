open Str

let convert ll = 
  (* Replace `load i32,` with `load` *)
  let lla = global_replace (regexp "load[^,]*, ") "load " ll in

  (* Replace `getelementptr [2 x i32],` with `getelementptr` *)
  let llb = global_replace (regexp "getelementptr inbounds [^,]*, ") "getelementptr inbounds " lla in

  (* Replace `smack_value* (i32*, ...) bitcast` with `smack_value* (i32*, ...)* bitcast` *)
  let llc = global_replace (regexp "smack_value\(.*\)) bitcast") "smack_value\1)* bitcast" llb in

  (* Remove lines with `!0` *)
  let lld = global_replace (regexp ".*!0.*") "" llc in

  (* Replace `norecurse`with `` *)
  global_replace (regexp "norecurse") "" lld
 
