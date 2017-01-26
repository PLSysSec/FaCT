CC = clang

all: build
	./test

build: stats benchmark target
	$(CC) stats.o benchmark.o -lm -o test

stats: stats.c
	$(CC) -c stats.c -o stats.o
	
	
benchmark: benchmark.c
	$(CC) -c benchmark.c -O0 -o benchmark.o

target: 	
	

clean:
	rm -f test *.o
