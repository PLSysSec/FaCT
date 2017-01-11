UNITY_ROOT=../Unity
CC=gcc
CFLAGS=-Wall -Wextra -Werror

INCLUDE=-I$(UNITY_ROOT)/src
SRC=harness.c
RUNNER=harness.c.runner.c
CSRC=test_semantics_fully_labeled.const
OBJ=test_semantics_fully_labeled.o

.PHONY: all run

all: run

$(OBJ): $(CSRC)
	../constanc.byte $(CSRC)

$(RUNNER): $(SRC)
	ruby $(UNITY_ROOT)/auto/generate_test_runner.rb $(SRC) $(RUNNER)

test: $(OBJ) $(SRC) $(RUNNER)
	$(CC) $(CFLAGS) $(INCLUDE) $(UNITY_ROOT)/src/unity.c $(OBJ) $(SRC) $(RUNNER) -o test

run: test
	./test

clean:
	rm -f *.o *.bc *.s *.runner.c test
