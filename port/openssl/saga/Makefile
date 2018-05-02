all:

nice:
	cp 20110823_fast.fact saga.fact
	../../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O3 saga.fact
	sed -i -e '/declare.*_structcopy/d' saga.ll
	cat saga.ll structcopy.ll > saga2.ll
	opt -S -O3 saga2.ll -o - > saga.ll
	llc -O3 -mcpu=core-avx2 saga.ll

ghetto:
	cp 20170717_latest.fact saga.fact
	../../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O3 saga.fact
	sed -i -e '/declare.*_structcopy/d' saga.ll
	cat saga.ll structcopy.ll > saga2.ll
	opt -S -O3 saga2.ll -o - > saga.ll
	llc -O3 -mcpu=core-avx2 saga.ll

raw:
	cp 20170717_latest_raw.fact saga.fact
	../../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O3 saga.fact
	sed -i -e '/declare.*_structcopy/d' saga.ll
	cat saga.ll structcopy.ll > saga2.ll
	opt -S -O3 saga2.ll -o - > saga.ll
	llc -O3 -mcpu=core-avx2 saga.ll

.PHONY: all copy

20110823_fast.o: 20110823_fast.fact Makefile ../../../fact.byte
	../../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O2 20110823_fast.fact

20170717_latest.o: 20170717_latest.fact Makefile ../../../fact.byte
	../../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O2 20170717_latest.fact

20170717_latest_raw.o: 20170717_latest_raw.fact Makefile ../../../fact.byte
	../../../fact.byte -pseudocode -generate-header -ast-out -core-ir-out -llvm-out -debug -opt O2 20170717_latest_raw.fact


clean:
	rm -f *.s *.o *.bc 20170717_latest_raw.h *.pseudo.fact *.bin *.res *.ml
