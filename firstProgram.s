	.file	"dimlProgram"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4607182418800017408
	.text
	.align	16, 0x90
	.type	nest,@function
nest:
	vmovsd	.LCPI0_0(%rip), %xmm0
	jmp	nested
.Ltmp0:
	.size	nest, .Ltmp0-nest

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI1_0:
	.quad	4607182418800017408
	.text
	.align	16, 0x90
	.type	nested,@function
nested:
	vaddsd	.LCPI1_0(%rip), %xmm0, %xmm0
	ret
.Ltmp1:
	.size	nested, .Ltmp1-nested

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4607182418800017408
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	.cfi_startproc
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
printInt:
	.cfi_startproc
	pushq	%rax
.Ltmp6:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rcx
	movl	$.L.fmti, %edi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	callq	printf
	popq	%rax
	ret
.Ltmp7:
	.size	printInt, .Ltmp7-printInt
	.cfi_endproc

	.type	.L.fmti,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	"%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
