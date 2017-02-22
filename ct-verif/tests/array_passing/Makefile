########## EDIT THE SECTION BELOW ##########
ENTRYPOINTS  = mutateIfNot5_wrapper # the name of the function to verify
INPUT       ?= test_array_passing-3.5.ll # the file containing the function to verify (a .ll or .c file)
#INPUT       ?= test_array_passing.c # the file containing the function to verify (a .ll or .c file)
WRAPPER     ?= wrapper.c  # the wrapper function (a .c file)
############################################

export ENTRYPOINTS
export WD          = ${PWD}
export INPUTFILE   = $(WD)/$(INPUT)
export WRAPPERFILE = $(WD)/$(WRAPPER)

all: 
	make -C ../

clean:
	rm -f *.bpl *.bc


