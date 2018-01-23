all: s3_cbc.o

.PRECIOUS: s3_cbc.h

s3_cbc.o: stdlib.fact s3_cbc.fact Makefile ../../fact.byte
	../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug stdlib.fact s3_cbc.fact


clean:
	rm -f *.s *.o *.bc s3_cbc.h *.pseudo.fact *.ll *.bin *.res *.ml
