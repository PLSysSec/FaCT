CC = clang
BENCHMARK_FLAGS ?= 
EXE_FILE ?= test.out
EXE ?= $(TARGETDIR)/$(EXE_FILE)
OUT_FILE ?= $(TARGETDIR)/results.txt

HAS_RDTSCP = $(shell grep "rdtscp" /proc/cpuinfo | wc -l)
ifeq ($(HAS_RDTSCP),0)
	DEF_TIMER = -DRDTSC
else
	DEF_TIMER = -DRDTSCP
endif

# taskset -c 0 ties the process to core 0; this guarantees consistent hardware across different tests
all: $(EXE)
	taskset -c 0 $(EXE) >> $(OUT_FILE)

# build
$(EXE): stats.o benchmark.o $(TARGETDIR)/$(O_FILE)
	$(CC) stats.o benchmark.o -lm $(TARGETDIR)/$(O_FILE) -o $(EXE)

# stats
stats.o: stats.c
	$(CC) -c stats.c -o stats.o

# benchmark
benchmark.o: benchmark.c
	$(CC) -c benchmark.c -O0 -I$(TARGETDIR) -DTARGET_LIB=\"$(H_FILE)\" $(DEF_TIMER) $(BENCHMARK_FLAGS) -o benchmark.o

# target
$(TARGETDIR)/$(O_FILE): $(TARGETDIR)/$(C_FILE)
	$(CC) -c -O0 $(TARGETDIR)/$(C_FILE) -o $(TARGETDIR)/$(O_FILE)

clean:
	rm -f $(EXE) *.o
