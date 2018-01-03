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

val verify : (description * details, opt_pass_name) Hashtbl.t
          -> opt_pass_name
          -> Llvm.llmodule
          -> unit

