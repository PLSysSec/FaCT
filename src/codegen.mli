open Llvm
open Cast

exception Error of string

val codegen : llcontext -> llmodule -> cmodule -> unit
