VERIFDIR          ?=$(WD)/../../verifying-constant-time
BINDIR            ?=$(VERIFDIR)/bin
EXAMPLESDIR       ?=$(VERIFDIR)/examples
SMACKDIR          ?=$(VERIFDIR)/tools/smack
SMACKINCLUDEDIR   ?=$(SMACKDIR)/share/smack/include
SMACKCFILE        ?=$(SMACKDIR)/share/smack/lib/smack.c
SMACKBC           ?=$(WD)/smack.bc
INPUTBASE         ?=$(basename $(INPUTFILE))
INPUTEXT          ?=$(suffix $(INPUTFILE))
INPUTBC           ?=$(INPUTBASE).bc
FINALBC           ?=$(INPUTBASE)-final.bc
FINALBCNOPATH     ?=$(notdir $(FINALBC))
WRAPPERBC         ?=$(basename $(WRAPPERFILE)).bc

all: verify

smack:
	clang-3.5 -c -emit-llvm -O0 -I$(SMACKINCLUDEDIR) $(SMACKCFILE) -o $(SMACKBC)

wrapper:
	clang-3.5 -c -emit-llvm -O0 -I$(SMACKINCLUDEDIR) -I$(EXAMPLESDIR) $(WRAPPERFILE) -o $(WRAPPERBC)

link: smack wrapper
ifeq ($(INPUTEXT),.c)
	clang-3.5 -c -emit-llvm -O0 $(INPUTFILE) -o $(INPUTBC)
else ifeq ($(INPUTEXT),.ll)
	llvm-as-3.5 $(INPUTFILE) -o $(INPUTBC)
endif
	llvm-link-3.5 $(SMACKBC) $(INPUTBC) $(WRAPPERBC) -o $(FINALBC)

verify: link
	EXAMPLE=$(FINALBCNOPATH) make -C $(BINDIR) verify

dis: link
	llvm-dis-3.5 $(FINALBC)

clean:
	rm -f *.bpl *.bc
