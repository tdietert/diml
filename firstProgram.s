	.file	"dimlProgram"
	.text
	.align	16, 0x90
	.type	lambda,@function
lambda:
	vaddsd	%xmm2, %xmm0, %xmm0
	ret
.Ltmp0:
	.size	lambda, .Ltmp0-lambda

	.align	16, 0x90
	.type	s,@function
s:
	ret
.Ltmp1:
	.size	s, .Ltmp1-s

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4607182418800017408
.LCPI2_1:
	.quad	4613937818241073152
.LCPI2_2:
	.quad	-4616189618054758400
.LCPI2_3:
	.quad	-4611686018427387904
	.text
	.align	16, 0x90
	.type	fib,@function
fib:
	subq	$56, %rsp
	vmovaps	%xmm0, %xmm3
	vxorps	%xmm0, %xmm0, %xmm0
	vucomisd	%xmm0, %xmm3
	jne	.LBB2_1
	jnp	.LBB2_3
.LBB2_1:
	vucomisd	.LCPI2_1(%rip), %xmm3
	jae	.LBB2_4
	vmovsd	.LCPI2_0(%rip), %xmm0
.LBB2_3:
	addq	$56, %rsp
	ret
.LBB2_4:
	vaddsd	.LCPI2_2(%rip), %xmm3, %xmm0
	vmovaps	%xmm1, 16(%rsp)
	vmovsd	%xmm2, 40(%rsp)
	vmovsd	%xmm3, 8(%rsp)
	callq	fib
	vmovsd	%xmm0, 48(%rsp)
	vmovsd	8(%rsp), %xmm0
	vaddsd	.LCPI2_3(%rip), %xmm0, %xmm0
	vmovaps	16(%rsp), %xmm1
	vmovsd	40(%rsp), %xmm2
	callq	fib
	vmovsd	48(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	addq	$56, %rsp
	ret
.Ltmp2:
	.size	fib, .Ltmp2-fib

	.section	.rodata.cst16,"aM",@progbits,16
	.align	16
.LCPI3_0:
	.quad	4607182418800017408
	.quad	4611686018427387904
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI3_1:
	.quad	4617315517961601024
.LCPI3_2:
	.quad	4618441417868443648
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	pushq	%rax
	vmovaps	.LCPI3_0(%rip), %xmm1
	vmovsd	.LCPI3_1(%rip), %xmm0
	vmovsd	.LCPI3_2(%rip), %xmm2
	callq	fib
	vmovsd	%xmm0, (%rsp)
	vmovsd	.LCPI3_2(%rip), %xmm0
	vmovaps	.LCPI3_0(%rip), %xmm1
	vmovaps	%xmm0, %xmm2
	callq	lambda
	vmovsd	(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	vmovsd	%xmm0, (%rsp)
	vmovsd	.LCPI3_2(%rip), %xmm0
	vmovaps	.LCPI3_0(%rip), %xmm1
	vmovaps	%xmm0, %xmm2
	callq	s
	vmovsd	(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	popq	%rax
	ret
.Ltmp3:
	.size	main, .Ltmp3-main

	.globl	printInt
	.align	16, 0x90
	.type	printInt,@function
printInt:
	movq	%rdi, %rcx
	movl	$.L.fmti, %edi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	jmp	printf
.Ltmp4:
	.size	printInt, .Ltmp4-printInt

	.type	.L.fmti,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	"%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
