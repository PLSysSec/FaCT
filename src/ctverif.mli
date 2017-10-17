open Llvm
open Tast

(* Represents a ct_verif function *)
type t =
  | ASSUME
  | PUBLIC_IN
  | PUBLIC_OUT
  | DECLASSIFIED_OUT
  | SMACK_VALUE
  | SMACK_VALUES
  | SMACK_RETURN_VALUE

(* String version of a ct_verif type *)
val string_of_ct_verif : t -> string

(* Declare a prototype for a ct_verif function or type *)
val declare_ct_verif : llcontext -> llmodule -> t -> unit

val codegen_dec : variable_type -> llvalue -> llcontext -> llmodule -> llbuilder -> unit