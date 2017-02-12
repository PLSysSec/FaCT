UNITY_ROOT=../Unity
CC=gcc
CFLAGS=-Wall -Wextra -Werror

INCLUDE=-I$(UNITY_ROOT)/src
SRC=harness_sem.c
CSRC=test_semantics_fully_labeled.const
OBJ=$(patsubst %.const,%.o,$(CSRC))
RUNNER=$(patsubst %.c,%_runner.c,$(SRC))
TESTS=$(patsubst harness%.c,testbin%,$(SRC))

.PHONY: all run

all: run

%.o: %.const
	../constanc.byte -ast-out -core-ir-out $<

%_runner.c: %.c
	ruby $(UNITY_ROOT)/auto/generate_test_runner.rb $< $@

testbin%: $(OBJ) harness%.c harness%_runner.c
	$(CC) $(CFLAGS) $(INCLUDE) $(UNITY_ROOT)/src/unity.c $^ -o $@

run: $(TESTS)
	for x in testbin*; do ./$$x; done

clean:
	rm -f *.o *.bc *.s *_runner.c testbin* ast.txt tast.txt core_ir.txt
