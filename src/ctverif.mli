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
  | DISJOINT_REGIONS

(* String version of a ct_verif type *)
val string_of_ct_verif : t -> string

(* Declare a prototype for a ct_verif function or type *)
val declare_ct_verif : bool
                    -> llcontext
                    -> llmodule
                    -> t
                    -> unit

val codegen_dec : Codegen_utils.codegen_ctx_record
               -> variable_type
               -> llvalue
               -> unit

val generate_disjoint_regions : (Llvm.llvalue * Tast.lexpr') list
                             -> Codegen_utils.codegen_ctx_record
                             -> unit