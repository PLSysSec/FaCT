all: crypto_secretbox.o

crypto_secretbox.o: stdlib.fact crypto_poly1305.fact crypto_secretbox.fact
	../fact.byte -pseudocode -generate-header -llvm-out -debug $^

clean:
	rm -f *.s *.o *.bc *.h *.pseudo.fact *.ll
