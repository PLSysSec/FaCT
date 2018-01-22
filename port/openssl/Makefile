all: s3_cbc.o

.PRECIOUS: s3_cbc.h

s3_cbc.o: stdlib.fact s3_cbc.fact
	../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug $^


clean:
	rm -f *.s *.o *.bc s3_cbc.h *.pseudo.fact *.ll *.bin *.res *.ml
