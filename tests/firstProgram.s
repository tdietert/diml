	.file	"/home/thomasd/Documents/github/dimlCompiler/builtins/builtins.ll"
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
	.quad	4607182418800017408
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	pushq	%rax
	movl	$5, %edi
	callq	printInt
	vmovsd	.LCPI3_0(%rip), %xmm0
	popq	%rax
	ret
.Ltmp3:
	.size	main, .Ltmp3-main

	.type	.L.fmti,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	"%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
