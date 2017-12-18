all: salsa.bin poly1305.bin

.PRECIOUS: fact_%.h

fact_%.o: stdlib.fact fact_%.fact
	../../fact.byte -ast-out -pseudocode -generate-header -llvm-out -debug $^

fact_%.h: fact_%.o
	touch $@

c_%.o: c_%.c
	clang -g -c $< -o $@

main_%.o: %.c fact_%.h
	clang -g -c $< -o $@

%.bin: fact_%.o c_%.o main_%.o
	clang -g $^ -o $@

clean:
	rm -f *.s *.o *.bc fact_*.h *.pseudo.fact *.ll *.bin *.res *.ast.ml *.tast.ml
