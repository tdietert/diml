	.file	"dimlProgram"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4617315517961601024
	.text
	.align	16, 0x90
	.type	lambda,@function
lambda:
	vaddsd	.LCPI0_0(%rip), %xmm0, %xmm0
	ret
.Ltmp0:
	.size	lambda, .Ltmp0-lambda

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI1_0:
	.quad	4607182418800017408
.LCPI1_1:
	.quad	4613937818241073152
.LCPI1_2:
	.quad	-4616189618054758400
.LCPI1_3:
	.quad	-4611686018427387904
	.text
	.align	16, 0x90
	.type	fib,@function
fib:
	subq	$24, %rsp
	vmovaps	%xmm0, %xmm2
	vxorps	%xmm0, %xmm0, %xmm0
	vucomisd	%xmm0, %xmm2
	jne	.LBB1_1
	jnp	.LBB1_3
.LBB1_1:
	vucomisd	.LCPI1_1(%rip), %xmm2
	jae	.LBB1_4
	vmovsd	.LCPI1_0(%rip), %xmm0
.LBB1_3:
	addq	$24, %rsp
	ret
.LBB1_4:
	vaddsd	.LCPI1_2(%rip), %xmm2, %xmm0
	vmovsd	%xmm1, 8(%rsp)
	vmovsd	%xmm2, (%rsp)
	callq	fib
	vmovsd	%xmm0, 16(%rsp)
	vmovsd	(%rsp), %xmm0
	vaddsd	.LCPI1_3(%rip), %xmm0, %xmm0
	vmovsd	8(%rsp), %xmm1
	callq	fib
	vmovsd	16(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	addq	$24, %rsp
	ret
.Ltmp1:
	.size	fib, .Ltmp1-fib

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4617315517961601024
.LCPI2_1:
	.quad	4618441417868443648
.LCPI2_2:
	.quad	4607182418800017408
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	pushq	%rax
	vmovsd	.LCPI2_0(%rip), %xmm0
	vmovsd	.LCPI2_1(%rip), %xmm1
	callq	fib
	vmovsd	%xmm0, (%rsp)
	vmovsd	.LCPI2_1(%rip), %xmm0
	callq	lambda
	vmovsd	(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	vcvttsd2si	%xmm0, %rdi
	callq	print.tinteger
	vmovsd	.LCPI2_2(%rip), %xmm0
	popq	%rax
	ret
.Ltmp2:
	.size	main, .Ltmp2-main

	.globl	print.tinteger
	.align	16, 0x90
	.type	print.tinteger,@function
print.tinteger:
	movq	%rdi, %rcx
	movl	$.L.fmti, %edi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	jmp	printf
.Ltmp3:
	.size	print.tinteger, .Ltmp3-print.tinteger

	.type	.L.fmti,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	"%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
