
type opt_level = O0 | O1 | O2 | OF

val scalar_optimizations :
  (([ `Module ] Llvm.PassManager.t -> unit) * string) list

val vector_optimizations :
  (([ `Module ] Llvm.PassManager.t -> unit) * string) list

val ipo_optimizations :
  (([ `Module ] Llvm.PassManager.t -> unit) * string) list

val run_optimizations : opt_level -> Llvm.llmodule -> Llvm.llmodule

val create_pass_manager : unit -> [ `Module ] Llvm.PassManager.t

val run_optimization_pipeline : [ `Module ] Llvm.PassManager.t
                             -> Llvm.llmodule
                             -> bool

val add_optimization : ([ `Module ] Llvm.PassManager.t -> unit)
                    -> [ `Module ] Llvm.PassManager.t
                    -> unit

val verify_some_opts : Llvm.llmodule -> string list -> unit

val verify_opts : Llvm.llmodule -> bool