run: micro
	./micro

.PHONY: run

micro: ../../../../fact.byte micro.fact micro.c inteltime.h micro_c.c
	../../../../fact.byte -pseudocode -llvm-out -generate-header -opt O2 micro.fact
	clang-3.8 -Wall -Werror -O2 micro.c micro_c.c micro.o -o micro
