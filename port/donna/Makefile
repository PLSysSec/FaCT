all: curve25519.o

test: test_curve25519
	./test_curve25519 | head -123456 | tail -1

.PRECIOUS: curve25519.h

curve25519.o: stdlib.fact curve25519.fact Makefile ../../fact.byte
	../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O2 stdlib.fact curve25519.fact

test_curve25519: test_curve25519.c curve25519.o
	gcc -o test_curve25519 curve25519.o test_curve25519.c  $(CFLAGS) $(CFLAGS_32)

clean:
	rm -f *.s *.o *.bc s3_cbc.h *.pseudo.fact *.ll *.bin *.res *.ml test_curve25519