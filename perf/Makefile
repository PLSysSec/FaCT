all: cond_sel.h salsa_fact.h cond_sel_fact0.bin cond_sel_c0.bin cond_sel_c0.bin cond_sel_fact1.bin cond_sel_fact2.bin cond_sel_c1.bin cond_sel_c2.bin salsa_c0.bin salsa_c1.bin salsa_c2.bin salsa_fact0.bin salsa_fact1.bin salsa_fact2.bin runner

clean:
	rm -f *.o *.s *.bc *.ll *.pseudo.fact *.bin cond_sel*.h salsa*.h fact*.h

.PHONY: all clean

runner: cond_sel_fact0.bin cond_sel_c0.bin cond_sel_fact1.bin cond_sel_fact2.bin cond_sel_c1.bin cond_sel_c2.bin salsa_fact0.bin salsa_fact1.bin salsa_fact2.bin
	python run.py

benchmark.o: benchmark.c
	clang -c -O2 -o $@ $<

# START CONDITIONAL SELECT
cond_sel_fact0.o: cond_sel.fact
	../fact.byte -generate-header -llvm-out -o $@ $^

cond_sel_fact1.o: cond_sel.fact
	../fact.byte -generate-header -llvm-out -opt O1 -o $@ $^

cond_sel_fact2.o: cond_sel.fact
	../fact.byte -generate-header -llvm-out -opt O2 -o $@ $^

cond_sel.h: cond_sel_fact0.o
	mv cond_sel_fact0.h $@

cond_sel_impl0.o: cond_sel_impl.c
	clang -c -O0 -o $@ $<

cond_sel_impl1.o: cond_sel_impl.c
	clang -c -O1 -o $@ $<

cond_sel_impl2.o: cond_sel_impl.c
	clang -c -O2 -o $@ $<

cond_sel.o: cond_sel.c
	clang -c -O2 -o $@ $<

cond_sel_fact0.bin: benchmark.o cond_sel_fact0.o cond_sel.o
	clang -o $@ $^

cond_sel_fact1.bin: benchmark.o cond_sel_fact1.o cond_sel.o
	clang -o $@ $^

cond_sel_fact2.bin: benchmark.o cond_sel_fact2.o cond_sel.o
	clang -o $@ $^

cond_sel_c0.bin: benchmark.o cond_sel_c0.o cond_sel_impl0.o
	clang -o $@ $^

cond_sel_c1.bin: benchmark.o cond_sel_c1.o cond_sel_impl1.o
	clang -o $@ $^

cond_sel_c2.bin: benchmark.o cond_sel_c2.o cond_sel_impl2.o
	clang -o $@ $^

# END CONDITIONAL SELECT

# START SALSA

salsa_fact0.o: ../port/crypto_secretbox/testing/stdlib.fact ../port/crypto_secretbox/testing/fact_salsa.fact
	../fact.byte -generate-header -llvm-out -o $@ $^

salsa_fact1.o: ../port/crypto_secretbox/testing/stdlib.fact ../port/crypto_secretbox/testing/fact_salsa.fact
	../fact.byte -generate-header -llvm-out -opt O1 -o $@ $^

salsa_fact2.o: ../port/crypto_secretbox/testing/stdlib.fact ../port/crypto_secretbox/testing/fact_salsa.fact
	../fact.byte -generate-header -llvm-out -opt O2 -o $@ $^

salsa_fact.h: salsa_fact0.o
	mv salsa_fact0.h $@

c_salsa0.o: ../port/crypto_secretbox/testing/c_salsa.c
	clang -c -O0 -o $@ $<

c_salsa1.o: ../port/crypto_secretbox/testing/c_salsa.c
	clang -c -O1 -o $@ $<

c_salsa2.o: ../port/crypto_secretbox/testing/c_salsa.c
	clang -c -O2 -o $@ $<

salsa_wrapper.o: salsa_wrapper.c
	clang -c -O2 -o $@ $<

salsa_fact0.bin: benchmark.o salsa_fact0.o salsa_wrapper.o
	clang -O0 -o $@ $^

salsa_fact1.bin: benchmark.o salsa_fact1.o salsa_wrapper.o
	clang -O0 -o $@ $^

salsa_fact2.bin: benchmark.o salsa_fact2.o salsa_wrapper.o
	clang -O0 -o $@ $^

salsa_c0.bin: benchmark.o salsa_wrapper.o c_salsa0.o
	clang -O0 -o $@ $^

salsa_c1.bin: benchmark.o salsa_wrapper.o c_salsa1.o
	clang -O0 -o $@ $^

salsa_c2.bin: benchmark.o salsa_wrapper.o c_salsa2.o
	clang -O0 -o $@ $^

# END SALSA
