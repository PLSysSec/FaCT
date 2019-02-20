all: salsa.bin

.PRECIOUS: fact_%.h

fact_%.o: fact_%.fact
	../../factc -ast-out -pseudocode -generate-header -llvm-out -debug $^

fact_%.h: fact_%.o
	touch $@

main_%.o: %.c fact_%.h
	clang -g -c $< -o $@

%.bin: fact_%.o main_%.o
	clang -g $^ -o $@

clean:
	rm -f *.s *.o *.bc fact_*.h *.pseudo.fact *.ll *.bin *.res *.ast.ml *.tast.ml
