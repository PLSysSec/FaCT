open Llvm
external ident: 'a -> 'a = "caml_ident"

external custom_create_module : llcontext -> string -> llmodule = "custom_llvm_create_module"

external cost_model_pass
  : [< Llvm.PassManager.any ] Llvm.PassManager.t -> unit
  = "llvm_add_cost_pass"