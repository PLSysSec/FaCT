UNITY_ROOT=../Unity
CC=gcc
CFLAGS=-Wall -Wextra -Werror

INCLUDE=-I$(UNITY_ROOT)/src

# test_%.fact
CSRC=$(filter-out $(wildcard *.pseudo.fact), $(wildcard *.fact))

# harness_%.c
SRC=$(patsubst test_%.fact,harness_%.c,$(CSRC))

# test_%.o
OBJ=$(patsubst %.fact,%.o,$(CSRC))

# harness_%_runner.c
RUNNER=$(patsubst %.c,%_runner.c,$(SRC))

# testbin_%
TESTS=$(patsubst harness%.c,testbin%,$(SRC))

.PHONY: all run

all: $(TESTS) run failure

# test_%.fact --> test_%.o 
%.o: %.fact
	../fact.byte -debug -ast-out -core-ir-out -llvm-out -pseudocode -generate-header $<

# harness_%.c --> harness_%_runner.c 
%_runner.c: %.c
	ruby $(UNITY_ROOT)/auto/generate_test_runner.rb $< $@

# all --> testbin_% 
testbin_%: test_%.o harness_%.c harness_%_runner.c
	$(CC) $(CFLAGS) $(INCLUDE) $(UNITY_ROOT)/src/unity.c $^ -o $@

run:
	for x in testbin_*; do ./$$x; done

FAILURES_DIR=failures
failure:
	@for x in $(FAILURES_DIR)/*.fact; do ../fact.byte $$x 2> /dev/null && { echo "ERROR: Build succeeded but should not have -- $$x"; exit 1; } || echo "Build failed as expected -- $$x" ; done

clean:
	rm -f *.o *.bc *.s *_runner.c testbin* *.ast.ml *.tast.ml *.core.ml *.ll *.pseudo.fact *.xftast.ml *.h $(FAILURES_DIR)/*.o $(FAILURES_DIR)/*.bc $(FAILURES_DIR)/*.s
	
