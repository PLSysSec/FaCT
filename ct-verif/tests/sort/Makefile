########## EDIT THE SECTION BELOW ##########
ENTRYPOINTS    = sort3_wrapper # the name of the function to verify
#INPUT         ?= sort.c # the file containing the function to verify (a .ll or .c file)
#WRAPPER       ?= sort-c-wrapper.c  # the wrapper function (a .c file)
INPUT         ?= sort-3.5.ll # the file containing the function to verify (a .ll or .c file)
WRAPPER       ?= sort-bc-wrapper.c  # the wrapper function (a .c file)
############################################

export ENTRYPOINTS
export WD          = ${PWD}
export INPUTFILE   = $(WD)/$(INPUT)
export WRAPPERFILE = $(WD)/$(WRAPPER)

all: 
	make -C ../

clean:
	rm -f *.bpl *.bc
	make -C ../ clean
