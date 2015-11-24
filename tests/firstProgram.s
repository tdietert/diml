	.file	"dimlProgram"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4622382067542392832
	.text
	.align	16, 0x90
	.type	f,@function
f:
	vmovaps	%xmm1, %xmm2
	vmovaps	%xmm0, %xmm1
	vmovsd	.LCPI0_0(%rip), %xmm0
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
	subq	$40, %rsp
	vucomisd	.LCPI2_0(%rip), %xmm0
	jae	.LBB2_1
	vmovsd	.LCPI2_3(%rip), %xmm0
	addq	$40, %rsp
	ret
.LBB2_1:
	vmovsd	%xmm0, 8(%rsp)
	vaddsd	.LCPI2_1(%rip), %xmm0, %xmm0
	vmovsd	%xmm1, 24(%rsp)
	vmovsd	%xmm2, 16(%rsp)
	callq	fib
	vmovsd	%xmm0, 32(%rsp)
	vmovsd	8(%rsp), %xmm0
	vaddsd	.LCPI2_2(%rip), %xmm0, %xmm0
	vmovsd	24(%rsp), %xmm1
	vmovsd	16(%rsp), %xmm2
	callq	fib
	vmovsd	32(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	addq	$40, %rsp
	ret
.Ltmp2:
	.size	fib, .Ltmp2-fib

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI3_0:
	.quad	4621819117588971520
.LCPI3_1:
	.quad	4607182418800017408
.LCPI3_2:
	.quad	4611686018427387904
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	vmovsd	.LCPI3_0(%rip), %xmm0
	vmovsd	.LCPI3_1(%rip), %xmm1
	vmovsd	.LCPI3_2(%rip), %xmm2
	callq	fib
	vmovsd	%xmm0, (%rsp)
	vmovsd	.LCPI3_1(%rip), %xmm0
	vmovsd	.LCPI3_2(%rip), %xmm1
	callq	f
	vmovsd	(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	vcvttsd2si	%xmm0, %rdi
	callq	printInt
	vmovsd	.LCPI3_1(%rip), %xmm0
	popq	%rax
	ret
.Ltmp5:
	.size	main, .Ltmp5-main
	.cfi_endproc

	.globl	fst
	.align	16, 0x90
	.type	fst,@function
fst:
	.cfi_startproc
	ret
.Ltmp6:
	.size	fst, .Ltmp6-fst
	.cfi_endproc

	.globl	snd
	.align	16, 0x90
	.type	snd,@function
snd:
	.cfi_startproc
	vmovaps	%xmm1, %xmm0
	ret
.Ltmp7:
	.size	snd, .Ltmp7-snd
	.cfi_endproc

	.globl	printInt
	.align	16, 0x90
	.type	printInt,@function
printInt:
	.cfi_startproc
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rcx
	movl	$.L.fmti, %edi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	callq	printf
	popq	%rax
	ret
.Ltmp10:
	.size	printInt, .Ltmp10-printInt
	.cfi_endproc

	.type	.L.fmti,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	"%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
