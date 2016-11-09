
(** Compile a constantc program. *)
val compile: Ast.constantc_module -> unit

(** Runs a constantc LLVM IR. The IR must be in the current working driectory and be named `out.ll` *)
val run: unit -> unit
