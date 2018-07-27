open Llvm_scalar_opts
open Llvm_vectorize
open Llvm_ipo

(* All of the optimizations *)
type optimization =
    (* Needed for graph generation *)
  | NoOptimization

    (* Scalars *)
  | AggressiveDCE
  | AlignmentFromAssumptions
  | CFGSimplification
  | DeadStoreElimination
  | Scalarizer
  | MergedLoadStoreMotion
  | GVN
  | IndVarSimplification
  | InstructionCombination
  | JumpThreading
  | LICM
  | LoopDeletion
  | LoopIdiom
  | LoopRotation
  | LoopReroll
  | LoopUnswitch
  | MemcpyOpt
  | PartiallyInlineLibCalls
  | LowerSwitch
  | MemoryToRegisterPromotion
  | Reassociation
  | SCCP
  | ScalarReplAggregation
  | ScalarReplAggregationSSA
  | ScalarReplAggregationWithThreshold
  | LibCallSimplification
  | TailCallElimination
  | ConstantPropogation
  | MemoryToRegisterDemotion
  | Verifier
  | CorrelatedValuePropogation
  | EarlyCSE
  | LoweExpectIntrinsic
  | TypeBasedAliasAnalysis
  | ScopedNoAliasAnalysis
  | BasicAliasAnalysis

    (* Vectors *)
  | BBVectorize
  | LoopVectorize
  | SLPVectorize

    (* IPO *)
  | ArgumentPromotion
  | ConstantMerge
  | DeadArgElimination
  | FunctionAttrs
  | FunctionInlining
  | AlwaysInliner
  | GlobalDCE
  | GlobalOptimizer
  | IPCPropogation
  | PruneEH
  | IPSCCP
  | Internalize
  | StripDeadPrototypes
  (*| StripSymbols*)
[@@deriving show,eq]
  (* ... *)

let opts = [
  (* Scalars *)
  AggressiveDCE;
  AlignmentFromAssumptions;
  CFGSimplification;
  DeadStoreElimination;
  Scalarizer;
  MergedLoadStoreMotion;
  GVN;
  IndVarSimplification;
  InstructionCombination;
  JumpThreading;
  LICM;
  LoopDeletion;
  LoopIdiom;
  LoopRotation;
  LoopReroll;
  LoopUnswitch;
  MemcpyOpt;
  PartiallyInlineLibCalls;
  LowerSwitch;
  MemoryToRegisterPromotion;
  Reassociation;
  SCCP;
  ScalarReplAggregation;
  ScalarReplAggregationSSA;
  ScalarReplAggregationWithThreshold;
  LibCallSimplification;
  TailCallElimination;
  ConstantPropogation;
  MemoryToRegisterDemotion;
  Verifier;
  CorrelatedValuePropogation;
  EarlyCSE;
  LoweExpectIntrinsic;
  TypeBasedAliasAnalysis;
  ScopedNoAliasAnalysis;
  BasicAliasAnalysis;

  (* Vectors *)
  BBVectorize;
  LoopVectorize;
  SLPVectorize;

  (* IPO *)
  ArgumentPromotion;
  ConstantMerge;
  DeadArgElimination;
  FunctionAttrs;
  FunctionInlining;
  AlwaysInliner;
  GlobalDCE;
  GlobalOptimizer;
  IPCPropogation;
  PruneEH;
  IPSCCP;
  Internalize;
  StripDeadPrototypes;
  (*StripSymbols*)
]
(* In order to have an effect, some optimizations must occur in a certain order.
   This type lets us enumerate these cases to provide a better search. *)
type dependency =
    (* These optimizations must occur next to each other *)
  | Adjacent of optimization * optimization
    (* These optimizations must occur in this order but can have other
       optimizations in between them *)
  | InOrder of optimization * optimization


(* A pipeline is just an in order list of optimizations to run *)
type pipeline = optimization list

(* The optimization that introduced the vulnerability *)
type troublemaker = optimization

(* The optimization that allowed the troublemaker to introduce the vulnerability *)
type source = optimization

(* When errors are introduced, we need to find out why it happened. This
   involves finding all the culprits involved. Some optimizations leverage
   others in order to do some bad action. We need to figure this out. *)
type hypothesis =
  | PartialHypothesis of troublemaker * pipeline
  | SolvedHypothesis of troublemaker * source

type hypotheses = hypothesis list


(*
  1. Driver
    - Get next pipeline
    - Verify pipeline
    - Create hypotheses
    - Solve hyptoheses
    - If time up, pick pipeline

  2. Pipeline generator
    - Given Solved hypotheses, generate a unique pipeline
  
  3. Hypothesis generator
    - Given errors from a pipeline pass, create partial hypotheses
  
  4. Hypothesis Solver
    - Given a pipeline, partial hypotheses, and IR, solve each hypothesis


*)

let to_llopt = function
| NoOptimization -> None

(* Scalars *)
| AggressiveDCE ->
  Some (add_aggressive_dce, "add_aggressive_dce")
| AlignmentFromAssumptions ->
  Some (add_alignment_from_assumptions, "add_alignment_from_assumptions")
| CFGSimplification ->
  Some (add_cfg_simplification, "add_cfg_simplification")
| DeadStoreElimination ->
  Some (add_dead_store_elimination, "add_dead_store_elimination")
| Scalarizer ->
  Some (add_scalarizer, "add_scalarizer")
| MergedLoadStoreMotion ->
  Some (add_merged_load_store_motion, "add_merged_load_store_motion")
| GVN ->
  Some (add_gvn, "add_gvn")
| IndVarSimplification ->
  Some (add_ind_var_simplification, "add_ind_var_simplification")
| InstructionCombination ->
  Some (add_instruction_combination, "add_instruction_combination")
| JumpThreading ->
  Some (add_jump_threading, "add_jump_threading")
| LICM ->
  Some (add_licm, "add_licm")
| LoopDeletion ->
  Some (add_loop_deletion, "add_loop_deletion")
| LoopIdiom ->
  Some (add_loop_idiom, "add_loop_idiom")
| LoopRotation ->
  Some (add_loop_rotation, "add_loop_rotation")
| LoopReroll ->
  Some (add_loop_reroll, "add_loop_reroll")
| LoopUnswitch ->
  Some (add_loop_unswitch, "add_loop_unswitch")
| MemcpyOpt ->
  Some (add_memcpy_opt, "add_memcpy_opt")
| PartiallyInlineLibCalls ->
  Some (add_partially_inline_lib_calls, "add_partially_inline_lib_calls")
| LowerSwitch ->
  Some (add_lower_switch, "add_lower_switch")
| MemoryToRegisterPromotion ->
  Some (add_memory_to_register_promotion, "add_memory_to_register_promotion")
| Reassociation ->
  Some (add_reassociation, "add_reassociation")
| SCCP ->
  Some (add_sccp, "add_sccp")
| ScalarReplAggregation ->
  Some (add_scalar_repl_aggregation, "add_scalar_repl_aggregation")
| ScalarReplAggregationSSA ->
  Some (add_scalar_repl_aggregation_ssa, "add_scalar_repl_aggregation_ssa")
| ScalarReplAggregationWithThreshold -> (*Some add_scalar_repl_aggregation_with_threshold*) None
| LibCallSimplification ->
  Some (add_lib_call_simplification, "add_lib_call_simplification")
| TailCallElimination ->
  Some (add_tail_call_elimination, "add_tail_call_elimination")
| ConstantPropogation ->
  Some (add_constant_propagation, "add_constant_propagation")
| MemoryToRegisterDemotion ->
  Some (add_memory_to_register_demotion, "add_memory_to_register_demotion")
| Verifier ->
  Some (add_verifier, "add_verifier")
| CorrelatedValuePropogation ->
  Some (add_correlated_value_propagation, "add_correlated_value_propagation")
| EarlyCSE ->
  Some (add_early_cse, "add_early_cse")
| LoweExpectIntrinsic ->
  Some (add_lower_expect_intrinsic, "add_lower_expect_intrinsic")
| TypeBasedAliasAnalysis ->
  Some (add_type_based_alias_analysis, "add_type_based_alias_analysis")
| ScopedNoAliasAnalysis ->
  Some (add_scoped_no_alias_alias_analysis, "add_scoped_no_alias_alias_analysis")
| BasicAliasAnalysis ->
  Some (add_basic_alias_analysis, "add_basic_alias_analysis")

(* Vectors *)
| BBVectorize ->
  Some (add_bb_vectorize, "add_bb_vectorize")
| LoopVectorize ->
  Some (add_loop_vectorize, "add_loop_vectorize")
| SLPVectorize ->
  Some (add_slp_vectorize, "add_slp_vectorize")

(* IPO *)
| ArgumentPromotion ->
  Some (add_argument_promotion, "add_argument_promotion")
| ConstantMerge ->
  Some (add_constant_merge, "add_constant_merge")
| DeadArgElimination ->
  Some (add_dead_arg_elimination, "add_dead_arg_elimination")
| FunctionAttrs ->
  Some (add_function_attrs, "add_function_attrs")
| FunctionInlining ->
  Some (add_function_inlining, "add_function_inlining")
| AlwaysInliner ->
  Some (add_always_inliner, "add_always_inliner")
| GlobalDCE ->
  Some (add_global_dce, "add_global_dce")
| GlobalOptimizer ->
  Some (add_global_optimizer, "add_global_optimizer")
| IPCPropogation ->
  Some (add_ipc_propagation, "add_ipc_propagation")
| PruneEH ->
  Some (add_prune_eh, "add_prune_eh")
| IPSCCP ->
  Some (add_ipsccp, "add_ipsccp")
| Internalize -> (* Some add_internalize*) None
| StripDeadPrototypes ->
  Some (add_strip_dead_prototypes, "add_strip_dead_prototypes")
(*| StripSymbols ->
  (*Some (add_strip_symbols, "add_strip_symbols")*) None*)