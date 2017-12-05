all: fact.bin c0.bin c2.bin f2.bin

clean:
	rm -f *.o *.s *.bc *.ll *.pseudo.fact *.bin perf.h

.PHONY: all clean

fact.o: perf.fact
	../fact.byte -generate-header -llvm-out -pseudocode -o $@ $^

perf.h: fact.o
	mv fact.h $@

cperf.o: cperf.c
	clang -c -O0 -o $@ $<

cperf2.o: cperf.c
	clang -c -O2 -o $@ $<

opt.ll: fact.ll
	opt-3.8 -S -O2 $< > $@

opt.o: opt.ll
	clang-3.8 -c -o $@ $<

benchmark.o: benchmark.c perf.h
	clang -c -O2 -o $@ $<

fact.bin: benchmark.o fact.o
	clang -o $@ $^

c0.bin: benchmark.o cperf.o
	clang -o $@ $^

c2.bin: benchmark.o cperf2.o
	clang -o $@ $^

f2.bin: benchmark.o opt.o
	clang -o $@ $^
