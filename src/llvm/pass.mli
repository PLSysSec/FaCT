open Llvm

val ident : 'a -> 'a
val custom_create_module : llcontext -> string -> llmodule

val cost_model_pass : [< Llvm.PassManager.any ] Llvm.PassManager.t -> unit