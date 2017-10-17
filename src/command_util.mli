type args_record = {
  in_file     : string;
  out_file    : string option;
  debug       : bool;
  ast_out     : bool;
  core_ir_out : bool;
  llvm_out    : bool;
  gen_header  : bool;
  verify_llvm : bool;
}

(** Compile a constantc program. *)
val compile : (string * string * string) -> args_record -> unit

(*
val run : unit -> unit
val link : unit -> unit
val assemble : unit -> unit
val share : unit -> unit
val clean : unit -> unit
val compile_harness : unit -> unit
val compile_c : string -> (unit -> unit)
val compile_ssl : unit -> unit

val run_command : string -> string array -> unit
*)
