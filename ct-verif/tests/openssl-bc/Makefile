########## EDIT THE SECTION BELOW ##########
export ENTRYPOINTS =ssl3_cbc_remove_padding_wrapper # the name of the function to verify
INPUTFILE         ?=ssl3_cbc_remove_padding.c # the file containing the function to verify (a .ll or .c file)
WRAPPERFILE       ?=ssl3_cbc_remove_padding_wrapper.c  # the wrapper function (a .c file)
############################################

export WD =${PWD}
VERIFDIR          ?=$(WD)/../../verifying-constant-time
BINDIR            ?=$(VERIFDIR)/bin
EXAMPLESDIR       ?=$(VERIFDIR)/examples
SMACKDIR          ?=$(VERIFDIR)/tools/smack
SMACKINCLUDEDIR   ?=$(SMACKDIR)/share/smack/include
SMACKCFILE        ?=$(SMACKDIR)/share/smack/lib/smack.c
INPUTBASE         ?=$(basename $(INPUTFILE))
INPUTEXT          ?=$(suffix $(INPUTFILE))
INPUTBC           ?=$(INPUTBASE).bc
FINALBC           ?=$(INPUTBASE)-final.bc
WRAPPERBC         ?=$(basename $(WRAPPERFILE)).bc

all: verify

verify:
ifeq ($(INPUTEXT),.c)
#	clang-3.5 -c -emit-llvm -O0 $(INPUTFILE) -o $(INPUTBC)
#	clang-3.5 -c -emit-llvm -O0 -I$(SMACKINCLUDEDIR) -I$(EXAMPLESDIR) $(WRAPPERFILE) -o $(WRAPPERBC)
#	llvm-link-3.5 $(INPUTBC) $(WRAPPERBC) -o tmp
#	clang-3.5 -c -emit-llvm -O0 -I$(SMACKINCLUDEDIR) $(SMACKCFILE) -o smack.bc
#	llvm-link-3.5 smack.bc $(INPUTBC) $(WRAPPERBC) -o $(FINALBC)
	ENTRYPOINTS=ssl3_cbc_remove_padding_wrapper EXAMPLE=$(INPUTFILE) make -C $(BINDIR) verify
else ifeq ($(INPUTEXT),.ll)
	llvm-as-3.5 $(INPUTFILE) -o $(INPUTBC)
	clang-3.5 -c -emit-llvm -O0 -I$(SMACKINCLUDEDIR) $(SMACKCFILE) -o smack.bc
	clang-3.5 -c -emit-llvm -O0 -I$(SMACKINCLUDEDIR) -I$(EXAMPLESDIR) $(WRAPPERFILE) -o $(WRAPPERBC)
	llvm-link-3.5 smack.bc $(INPUTBC) $(WRAPPERBC) -o $(FINALBC)
	EXAMPLE=$(FINALBC) make -C $(BINDIR) verify
endif

clean:
	rm -f *.bpl *.bc
