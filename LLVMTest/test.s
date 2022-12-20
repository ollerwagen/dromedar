	.text
	.file	"test.ll"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
# %bb.0:
	movl	$3, %eax
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
                                        # -- End function
	.globl	f                       # -- Begin function f
	.p2align	4, 0x90
	.type	f,@function
f:                                      # @f
# %bb.0:
	movl	$3, %eax
	retq
.Lfunc_end1:
	.size	f, .Lfunc_end1-f
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
	.addrsig
