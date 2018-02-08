all: secretbox1.bin secretbox2.bin crypto_secretbox_wrapper.fpic.o

.PRECIOUS: crypto_secretbox.h

crypto_secretbox.o: stdlib.fact crypto_poly1305.fact crypto_secretbox.fact
	../../fact.byte -ast-out -pseudocode -generate-header -llvm-out -debug $^

crypto_secretbox.h: crypto_secretbox.o
	touch $@

crypto_secretbox_wrapper.o: crypto_secretbox_wrapper.c crypto_secretbox.o
	clang -g -c $< -o $@

crypto_secretbox_wrapper.fpic.o: crypto_secretbox_wrapper.c crypto_secretbox.o
	clang -g -fPIC -c $< -o $@

secretbox%.o: secretbox%.c cmptest.h quirks.h crypto_secretbox_wrapper.h
	clang -g -c $< -o $@

secretbox%.bin: crypto_secretbox.o crypto_secretbox_wrapper.o secretbox%.o
	clang -g $^ -o $@

clean:
	rm -f *.s *.o *.bc crypto_secretbox.h *.pseudo.fact *.ll *.bin *.res *.ast.ml *.tast.ml *.xftast.ml
