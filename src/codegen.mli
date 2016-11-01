open Llvm
open Ast

val codegen : llcontext -> llmodule -> constantc_module -> unit
