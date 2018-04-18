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

type c_code = string

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

val generate_disjoint_regions : bool
                             -> (llvalue * Tast.lexpr' * Tast.var_name) list
                             -> Codegen_utils.codegen_ctx_record
                             -> (var_name * variable_type) Env.env
                             -> unit

val declassify : Codegen_utils.codegen_ctx_record
              -> llvalue
              -> unit

val generate_wrappers : string
                     -> Tast.fact_module
                     -> (Tast.fun_name * c_code) option list