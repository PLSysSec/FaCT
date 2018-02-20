SMACKDIR=/root/verifying-constant-time/tools/smack

all: verify

smack.bc: 
	clang -c -emit-llvm -O0 -g -gcolumn-info -I$(SMACKDIR)/share/smack/include -DMEMORY_MODEL_NO_REUSE_IMPLS $(SMACKDIR)/share/smack/lib/smack.c -o smack.bc

fact.bc: $(FACTLL)
	@test $(FACTLL) 2> /dev/null || { echo "ERROR: FACTLL not defined!"; exit 1; }
	llvm-as-3.5 $(FACTLL) -o fact.bc

linked.bc: smack.bc fact.bc
	llvm-link -o linked.bc smack.bc fact.bc

verify: linked.bc
	@test $(ENTRYPOINTS) 2> /dev/null || { echo "ERROR: ENTRYPOINTS not defined!"; exit 1; }
	EXAMPLE=linked.bc WD=${PWD} make -C /root/verifying-constant-time/bin/ verify

clean:
	rm -f *.bpl *.bc
