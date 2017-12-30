open Llvm_scalar_opts
open Llvm_vectorize
open Llvm_ipo

type opt_level = O0 | O1 | O2

type scalar_opts

let scalar_optimizations = [
  (add_aggressive_dce, "add_aggressive_dce");
  (add_alignment_from_assumptions, "add_alignment_from_assumptions");
  (add_cfg_simplification, "add_cfg_simplification");
  (add_dead_store_elimination, "add_dead_store_elimination");
  (add_scalarizer, "add_scalarizer");
  (add_merged_load_store_motion, "add_merged_load_store_motion");
  (add_gvn, "add_gvn");
  (add_ind_var_simplification, "add_ind_var_simplification");
  (add_instruction_combination, "add_instruction_combination");
  (add_jump_threading, "add_jump_threading");
  (add_licm, "add_licm");
  (add_loop_deletion, "add_loop_deletion");
  (add_loop_idiom, "add_loop_idiom");
  (add_loop_rotation, "add_loop_rotation");
  (add_loop_reroll, "add_loop_reroll");
  (add_loop_unroll, "add_loop_unroll");
  (add_loop_unswitch, "add_loop_unswitch");
  (add_memcpy_opt, "add_memcpy_opt");
  (add_partially_inline_lib_calls, "add_partially_inline_lib_calls");
  (add_lower_switch, "add_lower_switch");
  (add_memory_to_register_promotion, "add_memory_to_register_promotion");
  (add_reassociation, "add_reassociation");
  (add_sccp, "add_sccp");
  (add_scalar_repl_aggregation, "add_scalar_repl_aggregation");
  (add_scalar_repl_aggregation_ssa, "add_scalar_repl_aggregation_ssa");
  (*add_scalar_repl_aggregation_with_threshold; *)
  (add_lib_call_simplification, "add_lib_call_simplification");
  (add_tail_call_elimination, "add_tail_call_elimination");
  (add_constant_propagation, "add_constant_propagation");
  (add_memory_to_register_demotion, "add_memory_to_register_demotion");
  (add_verifier, "add_verifier");
  (add_correlated_value_propagation, "add_correlated_value_propagation");
  (add_early_cse, "add_early_cse");
  (add_lower_expect_intrinsic, "add_lower_expect_intrinsic");
  (add_type_based_alias_analysis, "add_type_based_alias_analysis");
  (add_scoped_no_alias_alias_analysis, "add_scoped_no_alias_alias_analysis");
  (add_basic_alias_analysis, "add_basic_alias_analysis");
]

let vector_optimizations = [
  (add_bb_vectorize, "add_bb_vectorize");
  (add_loop_vectorize, "add_loop_vectorize");
  (add_slp_vectorize, "add_slp_vectorize");
]

let ipo_optimizations = [
  (add_argument_promotion, "add_argument_promotion");
  (add_constant_merge, "add_constant_merge");
  (add_dead_arg_elimination, "add_dead_arg_elimination");
  (add_function_attrs, "add_function_attrs");
  (add_function_inlining, "add_function_inlining");
  (add_always_inliner, "add_always_inliner");
  (add_global_dce, "add_global_dce");
  (add_global_optimizer, "add_global_optimizer");
  (add_ipc_propagation, "add_ipc_propagation");
  (add_prune_eh, "add_prune_eh");
  (add_ipsccp, "add_ipsccp");
  (*add_internalize;*)
  (add_strip_dead_prototypes, "add_strip_dead_prototypes");
  (*(add_strip_symbols, "add_strip_symbols");*)
]

let run_optimizations opt_level llmodule =
  let pmb = Llvm_passmgr_builder.create () in
  let opt_level' = match opt_level with | O0 -> 0 | O1 -> 1 | O2 -> 2 in
  Llvm_passmgr_builder.set_opt_level opt_level' pmb;
  let pm = Llvm.PassManager.create () in
  Llvm_passmgr_builder.populate_module_pass_manager pm pmb;
  match Llvm.PassManager.run_module llmodule pm with
    | true -> ()
    | false -> Log.info "Optimization pass had no effect..."

let create_pass_manager () = Llvm.PassManager.create ()

let run_optimization_pipeline pm llmod = Llvm.PassManager.run_module llmod pm

let add_optimization opt pm = opt pm