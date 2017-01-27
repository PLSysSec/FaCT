CC = clang

all: build
	$(TARGETDIR)/test

build: stats benchmark target
	$(CC) stats.o benchmark.o -lm $(TARGETDIR)/$(O_FILE) -o $(TARGETDIR)/test

stats: stats.c
	$(CC) -c stats.c -o stats.o
	
	
benchmark: benchmark.c
	$(CC) -c benchmark.c -O0 -I$(TARGETDIR) -DTARGET_LIB=\"$(H_FILE)\" -o benchmark.o

target: $(TARGETDIR)/$(C_FILE)
	$(CC) -c $(TARGETDIR)/$(C_FILE) -o $(TARGETDIR)/$(O_FILE)

clean:
	rm -f test *.o
