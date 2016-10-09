	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3
LCPI0_0:
	.quad	4649069413771771904     ## double 666
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_my_function_name
	.p2align	4, 0x90
_my_function_name:                      ## @my_function_name
	.cfi_startproc
## BB#0:                                ## %entry
	movsd	LCPI0_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_world:                                ## @world
	.asciz	"hello"


.subsections_via_symbols
