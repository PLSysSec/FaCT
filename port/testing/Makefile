all: salsa.bin

fact_salsa.o: fact_salsa.fact
	../../fact.byte -pseudocode -generate-header -llvm-out -debug $^

c_salsa.o: c_salsa.c
	clang -g -c $< -o $@

salsa.o: salsa.c fact_salsa.h
	clang -g -c $< -o $@

salsa.bin: fact_salsa.o c_salsa.o salsa.o
	clang -g $^ -o $@

clean:
	rm -f *.s *.o *.bc fact_*.h *.pseudo.fact *.ll *.bin *.res
