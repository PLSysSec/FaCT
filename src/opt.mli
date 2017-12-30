
type opt_level = O0 | O1 | O2

val scalar_optimizations :
  (([ `Module ] Llvm.PassManager.t -> unit) * string) list

val vector_optimizations :
  (([ `Module ] Llvm.PassManager.t -> unit) * string) list

val ipo_optimizations :
  (([ `Module ] Llvm.PassManager.t -> unit) * string) list

val run_optimizations : opt_level -> Llvm.llmodule -> unit

val create_pass_manager : unit -> [ `Module ] Llvm.PassManager.t

val run_optimization_pipeline : [ `Module ] Llvm.PassManager.t
                             -> Llvm.llmodule
                             -> bool

val add_optimization : ([ `Module ] Llvm.PassManager.t -> unit)
                    -> [ `Module ] Llvm.PassManager.t
                    -> unit