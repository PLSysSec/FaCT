(*
    FaCT optimization pipeline

    Problem: Compiler optimizations often introduce security vulnerabilities.
             We need a custom optimization pipeline that will produce fast code
             without any security vulnerabilities

    Invariants:
      1. Every IR value has a label prior to an optimization running
      2. Every IR value has a label prior to the checker running
      3. When generating the next pipeline, there are no partial hypothesis
      4. Each vertex only has 1 predecessor

    Variants:
      1. The IR can have securtiy vulnerabilites after an optimization
        -- But not after all of them have been run
      2. The IR can have unlabeled values after an optimization

    Special cases:
      1. An optimization can introduce security vulnerabilites
      2. An optimization can remove a security vulnerability
      3. Some optimizations depend on each other in order to have an effect

    Questions
      1. Which optimizations have dependencies?
      2. When do optimizations have the biggest effects?
      3. What makes one optimization pass quantitatively better than another?
        --perf?
        --code size?

    How do we find the best optimization pass for a given program?
      -- Hypothesis testing

    Methodology:
      1. Run an entire optimization pipeline
        - This will produce valid or invalid IR
        - If valid IR, we dont get a hypothesis
        - If invalid, we make a hypothesis that an optimization combination
          caused the bad IR
          -- We know the optimization that caused the error, but is there
             another that setup the optimization for failure?
      2. After solving all the hypothesis, use that knowledge to create the next
         optimization pipeline
        - Repeat #1
      3. Do this until done or time limit is reached

*)

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
  | StripSymbols
[@@deriving show, eq]

val opts : optimization list

val to_llopt : optimization
            -> (([ `Module ] Llvm.PassManager.t -> unit) * string) option