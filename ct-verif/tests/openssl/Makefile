########## EDIT THE SECTION BELOW ##########
ENTRYPOINTS    = ssl3_cbc_remove_padding_wrapper # the name of the function to verify
INPUT         ?= ssl3_cbc_remove_padding-const3.5.ll # the file containing the function to verify (a .ll or .c file)
WRAPPER       ?= wrapper-bc.c  # the wrapper function (a .c file)
#INPUT         ?= ssl3_cbc_remove_padding.c # the file containing the function to verify (a .ll or .c file)
#WRAPPER       ?= wrapper-c.c  # the wrapper function (a .c file)
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
