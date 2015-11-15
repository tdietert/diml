	.file	"fp.bc"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4607182418800017408     # double 1
	.text
	.align	16, 0x90
	.type	nest,@function
nest:                                   # @nest
# BB#0:                                 # %entry
	vmovsd	.LCPI0_0(%rip), %xmm0
	jmp	nested                  # TAILCALL
.Ltmp0:
	.size	nest, .Ltmp0-nest

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI1_0:
	.quad	4607182418800017408     # double 1
	.text
	.align	16, 0x90
	.type	nested,@function
nested:                                 # @nested
# BB#0:                                 # %entry
	vaddsd	.LCPI1_0(%rip), %xmm0, %xmm0
	ret
.Ltmp1:
	.size	nested, .Ltmp1-nested

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4607182418800017408     # double 1
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp3:
	.cfi_def_cfa_offset 16
	callq	nest
	vcvttsd2si	%xmm0, %rdi
	callq	printInt
	vmovsd	.LCPI2_0(%rip), %xmm0
	popq	%rax
	ret
.Ltmp4:
	.size	main, .Ltmp4-main
	.cfi_endproc

	.globl	printInt
	.align	16, 0x90
	.type	printInt,@function
printInt:                               # @printInt
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp6:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	movl	$.L.fmti, %edi
	movq	%rax, %rsi
	xorb	%al, %al
	callq	printf
	popq	%rax
	ret
.Ltmp7:
	.size	printInt, .Ltmp7-printInt
	.cfi_endproc

	.type	.L.fmti,@object         # @.fmti
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	 "%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
