	.file	"dimlProgram"
	.text
	.align	16, 0x90
	.type	f,@function
f:
	jmp	lambda
.Ltmp0:
	.size	f, .Ltmp0-f

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI1_0:
	.quad	4611686018427387904
	.text
	.align	16, 0x90
	.type	lambda,@function
lambda:
	vaddsd	.LCPI1_0(%rip), %xmm0, %xmm0
	ret
.Ltmp1:
	.size	lambda, .Ltmp1-lambda

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4611686018427387904
.LCPI2_1:
	.quad	-4616189618054758400
.LCPI2_2:
	.quad	-4611686018427387904
.LCPI2_3:
	.quad	4607182418800017408
	.text
	.align	16, 0x90
	.type	fib,@function
fib:
	subq	$56, %rsp
	vucomisd	.LCPI2_0(%rip), %xmm0
	jae	.LBB2_1
	vmovsd	.LCPI2_3(%rip), %xmm0
	addq	$56, %rsp
	ret
.LBB2_1:
	vmovsd	%xmm0, 8(%rsp)
	vaddsd	.LCPI2_1(%rip), %xmm0, %xmm0
	vmovsd	%xmm1, 40(%rsp)
	vmovaps	%xmm2, 16(%rsp)
	callq	fib
	vmovsd	%xmm0, 48(%rsp)
	vmovsd	8(%rsp), %xmm0
	vaddsd	.LCPI2_2(%rip), %xmm0, %xmm0
	vmovsd	40(%rsp), %xmm1
	vmovaps	16(%rsp), %xmm2
	callq	fib
	vmovsd	48(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	addq	$56, %rsp
	ret
.Ltmp2:
	.size	fib, .Ltmp2-fib

	.globl	fst
	.align	16, 0x90
	.type	fst,@function
fst:
	leaq	-8(%rsp), %rax
	ret
.Ltmp3:
	.size	fst, .Ltmp3-fst

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI4_0:
	.quad	4621819117588971520
.LCPI4_1:
	.quad	4687288231794835456
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	pushq	%rax
	vmovsd	.LCPI4_0(%rip), %xmm0
	vmovsd	.LCPI4_1(%rip), %xmm1
	callq	fib
	vmovsd	%xmm0, (%rsp)
	vmovsd	.LCPI4_1(%rip), %xmm0
	callq	f
	vmovsd	(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	popq	%rax
	ret
.Ltmp4:
	.size	main, .Ltmp4-main

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
