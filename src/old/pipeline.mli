val continue : bool ref
val drive : Llvm.llmodule -> unit Lwt.t
val pick_pipeline : unit -> unit

val pair_prune : Llvm.llmodule -> unit