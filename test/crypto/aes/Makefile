FACT=../../../../constanc.byte
CC=clang-3.9

all: aes_c

aes_c: aes.fact aes.c
	$(FACT) aes.fact -o aes_fact.o 
	$(CC) -c aes_test.c -o aes_test.o 
	$(CC) -c aes.c -o aes_c.o 
	$(CC) aes_test.o aes_c.o aes_fact.o -o aes_c.out 
	@printf "Running C test... " & ./aes_c.out

#aes_fact:
#	$(CC) -c aes_test.c -o aes_test.o
#	$(FACT) aes.fact -o aes-fact.o
#	$(CC) aes_test.o aes-fact.o -o aes-fact.out
#	@printf "Running FaCT test... " & ./aes-fact.out
        
clean:
	rm -f *.out *.o *.bc *.s
