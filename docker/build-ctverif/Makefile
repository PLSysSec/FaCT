SMACKDIR=/root/verifying-constant-time/tools/smack
SMACKLIBDIR=$(SMACKDIR)/share/smack/include
SMACKC=$(SMACKDIR)/share/smack/lib/smack.c
SMACKFLAGS=-O0 -g -gcolumn-info -DMEMORY_MODEL_NO_REUSE_IMPLS
CTVERIFLIBDIR=/root/verifying-constant-time/examples

# these should be passed to make
CTVWRAPPERC?=a
FACTLIB?=b
FACTLL?=c
ENTRYPOINTS?=d

CTVWRAPPERBC=$(basename $(CTVWRAPPERC)).bc
FACTBC=$(basename $(FACTLL)).bc
FINALBC=$(basename $(FACTLL))_linked.bc
FACTLIBDIR=$(dir $(FACTLIB))

# foo_wrapper.c + foo.h -> foo_wrapper.bc
	# clang -emit-llvm -c foo_wrapper.c -o foo_wrapper.bc
# foo.ll -> foo.bc
	# llvm-as-3.5 foo_3.5.ll -o foo_3.5.bc
# smack.c -> smack.bc
	# clang -c ...... smack.c -o smack.bc

# foo_wrapper.bc + foo.bc + smack.bc -> foo_linked.bc
	# llvm-link foo_wrapper.bc foo.bc smack.bc -o foo_linked.bc

all: verify

$(CTVWRAPPERBC): $(CTVWRAPPERC)
	clang -emit-llvm -c -I$(CTVERIFLIBDIR) -I$(SMACKLIBDIR) -I$(FACTLIBDIR) $(CTVWRAPPERC) -o $(CTVWRAPPERBC)

$(FACTBC): $(FACTLL)
	llvm-as-3.5 $(FACTLL) -o $(FACTBC)

smack.bc: $(SMACKC)
	clang -c -emit-llvm $(SMACKFLAGS) -I$(SMACKLIBDIR) $(SMACKC) -o smack.bc

$(FINALBC): $(CTVWRAPPERBC) $(FACTBC) smack.bc
	llvm-link $(CTVWRAPPERBC) $(FACTBC) smack.bc -o $(FINALBC)

verify: $(FINALBC)
	EXAMPLE=$(FINALBC) WD=${PWD} make -C /root/verifying-constant-time/bin/ verify

usage: 
	@echo "Define variables: CTVWRAPPERC FACTLIB FACTLL ENTRYPOINTS"

clean:
	rm -f *.bpl *.bc
