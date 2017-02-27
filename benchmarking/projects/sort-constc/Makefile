export TARGETDIR=${PWD}
export H_FILE=sort.h
export C_FILE=sort.c
#export O_FILE=sort-constc.object
export O_FILE=sort-constc.bc

all:
	make -C ../../src

clean:
	rm -f *.o test
	make -C ../../src clean
