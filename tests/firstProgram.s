	.file	"/home/tdietert/Documents/github/dimlCompiler/builtins/builtins.ll"
	.text
	.globl	fst
	.align	16, 0x90
	.type	fst,@function
fst:
	ret
.Ltmp0:
	.size	fst, .Ltmp0-fst

	.globl	snd
	.align	16, 0x90
	.type	snd,@function
snd:
	vmovaps	%xmm1, %xmm0
	ret
.Ltmp1:
	.size	snd, .Ltmp1-snd

	.globl	printInt
	.align	16, 0x90
	.type	printInt,@function
printInt:
	movq	%rdi, %rcx
	movl	$.L.fmti, %edi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	jmp	printf
.Ltmp2:
	.size	printInt, .Ltmp2-printInt

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI3_0:
	.quad	4619567317775286272
	.text
	.align	16, 0x90
	.type	f,@function
f:
	vmovsd	.LCPI3_0(%rip), %xmm0
	ret
.Ltmp3:
	.size	f, .Ltmp3-f

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI4_0:
	.quad	4607182418800017408
.LCPI4_1:
	.quad	4611686018427387904
.LCPI4_2:
	.quad	4617315517961601024
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	pushq	%rax
	vmovsd	.LCPI4_0(%rip), %xmm0
	vmovsd	.LCPI4_1(%rip), %xmm1
	callq	snd
	vmovsd	%xmm0, (%rsp)
	vmovsd	.LCPI4_0(%rip), %xmm0
	vmovsd	.LCPI4_1(%rip), %xmm1
	callq	fst
	vmovsd	(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	vmovsd	%xmm0, (%rsp)
	vmovsd	.LCPI4_2(%rip), %xmm0
	vmovsd	.LCPI4_0(%rip), %xmm1
	vmovsd	.LCPI4_1(%rip), %xmm2
	callq	f
	vmovsd	(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	vcvttsd2si	%xmm0, %rdi
	callq	printInt
	vmovsd	.LCPI4_0(%rip), %xmm0
	popq	%rax
	ret
.Ltmp4:
	.size	main, .Ltmp4-main

	.type	.L.fmti,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	"%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
