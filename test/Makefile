UNITY_ROOT=../Unity
CC=gcc
CFLAGS=-Wall -Wextra -Werror

INCLUDE=-I$(UNITY_ROOT)/src
CSRC=$(filter-out $(wildcard *.pseudo.fact), $(wildcard *.fact))
SRC=$(patsubst test_%.fact,harness_%.c,$(CSRC))
OBJ=$(patsubst %.fact,%.o,$(CSRC))
RUNNER=$(patsubst %.c,%_runner.c,$(SRC))
TESTS=$(patsubst harness%.c,testbin%,$(SRC))

.PHONY: all run

all: $(TESTS) run

%.o: %.fact
	../fact.byte -debug -ast-out -core-ir-out -llvm-out -pseudocode -generate-header $<

%_runner.c: %.c
	ruby $(UNITY_ROOT)/auto/generate_test_runner.rb $< $@

testbin_%: test_%.o harness_%.c harness_%_runner.c
	$(CC) $(CFLAGS) $(INCLUDE) $(UNITY_ROOT)/src/unity.c $^ -o $@

run:
	for x in testbin_*; do ./$$x; done

clean:
	rm -f *.o *.bc *.s *_runner.c testbin* *.ast.ml *.tast.ml *.core.ml *.ll *.pseudo.fact *.xftast.ml *.h
