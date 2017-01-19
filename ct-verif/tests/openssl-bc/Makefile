########## EDIT THE SECTION BELOW ##########
export ENTRYPOINTS =ssl3_cbc_remove_padding_wrapper # the name of the function to verify
INPUTFILE         ?=out-3.5.ll # the file containing the function to verify (a .ll or .c file)
WRAPPERFILE       ?=wrapper.c  # the wrapper function (a .c file)
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

smack:
	clang-3.5 -c -emit-llvm -O0 -I$(SMACKINCLUDEDIR) $(SMACKCFILE) -o smack.bc

wrapper:
	clang-3.5 -c -emit-llvm -O0 -I$(SMACKINCLUDEDIR) -I$(EXAMPLESDIR) $(WRAPPERFILE) -o $(WRAPPERBC)

link: smack wrapper
ifeq ($(INPUTEXT),.c)
	llvm-link-3.5 smack.bc $(WRAPPERBC) -o $(FINALBC)
else ifeq ($(INPUTEXT),.ll)
	llvm-as-3.5 $(INPUTFILE) -o $(INPUTBC)
	llvm-link-3.5 smack.bc $(INPUTBC) $(WRAPPERBC) -o $(FINALBC)
endif

verify: link
	EXAMPLE=$(FINALBC) make -C $(BINDIR) verify

dis: link
	llvm-dis-3.5 $(FINALBC)

clean:
	rm -f *.bpl *.bc
