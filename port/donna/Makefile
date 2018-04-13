all: curve25519.o

.PRECIOUS: curve25519.h

curve25519.o: stdlib.fact curve25519.fact Makefile ../../fact.byte
	../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O2 stdlib.fact curve25519.fact
clean:
	rm -f *.s *.o *.bc s3_cbc.h *.pseudo.fact *.ll *.bin *.res *.ml