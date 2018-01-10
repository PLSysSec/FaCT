#include "caml/mlvalues.h"
#include "caml/misc.h"
#include <caml/memory.h>

#include "llvm-c/Core.h"
#include "llvm-c/Support.h"

#include "pass_header.h"

#include <stdio.h>

#ifndef OPTION_PASSED
#error "CCOpt doesn't work"
#endif

CAMLprim value caml_ident (value vs)
{
  CAMLparam1(vs);
  CAMLreturn(vs);
};

/* llcontext -> string -> llmodule */
CAMLprim LLVMModuleRef custom_llvm_create_module(LLVMContextRef C, value ModuleID) {
  return LLVMModuleCreateWithNameInContext(String_val(ModuleID), C);
}


/* [<Llvm.PassManager.any] Llvm.PassManager.t -> unit */
CAMLprim value llvm_add_cost_pass(LLVMPassManagerRef PM) {
  LLVMAddCostModelAnalysisPass(PM);
  return Val_unit;
}