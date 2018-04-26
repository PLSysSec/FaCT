run: micro
	./micro

.PHONY: run

micro: ../../../../fact.byte micro.fact micro.c inteltime.h
	../../../../fact.byte -pseudocode -llvm-out -generate-header -opt O2 micro.fact
	clang-3.8 -Wall -Werror -O2 micro.c micro.o -o micro
