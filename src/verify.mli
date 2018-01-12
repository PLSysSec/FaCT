type opt_pass_name = string
[@@deriving show]

type description = string
[@@deriving show]

type details = string
[@@deriving show]

type reason = opt_pass_name * description * details
[@@deriving show]

exception VerifyError of reason

type checkerstatus =
  | Error of reason
[@@deriving show]

(* Used to express the state of a pipeline on a module.
   If the pipeline passes, then the last vertex should be labeled Secure,
   otherwise InSecure. This allows us to prioritize how the queue is processed.
   I hypothesize that optimizations will introduce more bugs than it eliminates.
   If truem then we dont want to spend much time processing pipelines that have
   reached an InSecure state *)
type state =
  | Secure
  | InSecure
  | Unchanged
  | Unknown
[@@deriving show]

val verify : (description * details, opt_pass_name) Hashtbl.t
          -> opt_pass_name
          -> Llvm.llmodule
          -> state

