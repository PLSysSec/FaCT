
type opt_level = O0 | O1 | O2

let run_optimizations opt_level llmodule =
  let pmb = Llvm_passmgr_builder.create () in
  let opt_level' = match opt_level with | O0 -> 0 | O1 -> 1 | O2 -> 2 in
  Llvm_passmgr_builder.set_opt_level opt_level' pmb;
  let pm = Llvm.PassManager.create () in
  Llvm_passmgr_builder.populate_module_pass_manager pm pmb;
  match Llvm.PassManager.run_module llmodule pm with
    | true -> ()
    | false -> Log.info "Optimization pass had no effect..."