FACT=../../../../constanc.byte
CC=clang-3.9

rot_13: rot_13_c rot_13_fact

rot_13_c:
	$(CC) -c rot-13_test.c -o rot-13_test.o 
	$(CC) -c rot-13.c -o rot-13-c.o 
	$(CC) rot-13_test.o rot-13-c.o -o rot-13-c.out 
	@printf "Running C test... " & ./rot-13-c.out

rot_13_fact:
	$(CC) -c rot-13_test.c -o rot-13_test.o
	$(FACT) rot-13.fact -o rot-13-fact.o
	$(CC) rot-13_test.o rot-13-fact.o -o rot-13-fact.out
	@printf "Running FaCT test... " & ./rot-13-fact.out
        
clean:
	rm -f *.out *.o *.bc *.s
