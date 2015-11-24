	.file	"test.bc"
	.text
	.align	16, 0x90
	.type	f,@function
f:                                      # @f
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
	vmovsd	%xmm0, 16(%rsp)
	vmovsd	%xmm2, 8(%rsp)
	vmovsd	%xmm1, (%rsp)
	vmovsd	(%rsp), %xmm1
	vmovsd	8(%rsp), %xmm2
	vmovsd	16(%rsp), %xmm0
	callq	lambda
	addq	$24, %rsp
	ret
.Ltmp2:
	.size	f, .Ltmp2-f
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI1_0:
	.quad	4611686018427387904     # double 2
	.text
	.align	16, 0x90
	.type	lambda,@function
lambda:                                 # @lambda
	.cfi_startproc
# BB#0:                                 # %entry
	vmovsd	%xmm0, -8(%rsp)
	vmovsd	%xmm2, -16(%rsp)
	vmovsd	%xmm1, -24(%rsp)
	vmovsd	-8(%rsp), %xmm0
	vaddsd	.LCPI1_0(%rip), %xmm0, %xmm0
	ret
.Ltmp3:
	.size	lambda, .Ltmp3-lambda
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4611686018427387904     # double 2
.LCPI2_1:
	.quad	4607182418800017408     # double 1
.LCPI2_2:
	.quad	-4616189618054758400    # double -1
.LCPI2_3:
	.quad	-4611686018427387904    # double -2
	.text
	.align	16, 0x90
	.type	fib,@function
fib:                                    # @fib
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$40, %rsp
.Ltmp5:
	.cfi_def_cfa_offset 48
	vmovsd	%xmm0, 32(%rsp)
	vmovsd	%xmm2, 24(%rsp)
	vmovsd	%xmm1, 16(%rsp)
	vmovsd	32(%rsp), %xmm0
	vucomisd	.LCPI2_0(%rip), %xmm0
	sbbl	%eax, %eax
	andl	$1, %eax
	vcvtsi2sdl	%eax, %xmm0, %xmm0
	vxorpd	%xmm1, %xmm1, %xmm1
	vucomisd	%xmm1, %xmm0
	je	.LBB2_2
# BB#1:
	vmovsd	.LCPI2_1(%rip), %xmm0
	addq	$40, %rsp
	ret
.LBB2_2:                                # %if.else
	vmovsd	16(%rsp), %xmm1
	vmovsd	24(%rsp), %xmm2
	vmovsd	32(%rsp), %xmm0
	vaddsd	.LCPI2_2(%rip), %xmm0, %xmm0
	callq	fib
	vmovsd	%xmm0, 8(%rsp)          # 8-byte Spill
	vmovsd	16(%rsp), %xmm1
	vmovsd	24(%rsp), %xmm2
	vmovsd	32(%rsp), %xmm0
	vaddsd	.LCPI2_3(%rip), %xmm0, %xmm0
	callq	fib
	vmovsd	8(%rsp), %xmm1          # 8-byte Reload
	vaddsd	%xmm0, %xmm1, %xmm0
	addq	$40, %rsp
	ret
.Ltmp6:
	.size	fib, .Ltmp6-fib
	.cfi_endproc

	.globl	fst
	.align	16, 0x90
	.type	fst,@function
fst:                                    # @fst
	.cfi_startproc
# BB#0:                                 # %entry
	vmovsd	%xmm0, -8(%rsp)
	ret
.Ltmp7:
	.size	fst, .Ltmp7-fst
	.cfi_endproc

	.globl	printInt
	.align	16, 0x90
	.type	printInt,@function
printInt:                               # @printInt
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	movl	$.L.fmti, %edi
	movq	%rax, %rsi
	xorb	%al, %al
	callq	printf
	popq	%rax
	ret
.Ltmp10:
	.size	printInt, .Ltmp10-printInt
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI5_0:
	.quad	4621819117588971520     # double 10
.LCPI5_1:
	.quad	4622382067542392832     # double 11
.LCPI5_2:
	.quad	4607182418800017408     # double 1
.LCPI5_3:
	.quad	4611686018427387904     # double 2
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$40, %rsp
.Ltmp12:
	.cfi_def_cfa_offset 48
	movabsq	$4611686018427387904, %rax # imm = 0x4000000000000000
	movq	%rax, 16(%rsp)
	movabsq	$4607182418800017408, %rax # imm = 0x3FF0000000000000
	movq	%rax, 8(%rsp)
	vmovsd	8(%rsp), %xmm1
	vmovsd	16(%rsp), %xmm2
	vmovsd	.LCPI5_0(%rip), %xmm0
	callq	fib
	vmovsd	8(%rsp), %xmm1
	vmovsd	16(%rsp), %xmm2
	vmovsd	.LCPI5_1(%rip), %xmm0
	callq	f
	vmovsd	.LCPI5_2(%rip), %xmm0
	vmovsd	.LCPI5_3(%rip), %xmm1
	callq	fst
	vcvttsd2si	%xmm0, %rdi
	callq	printInt
	vmovsd	.LCPI5_2(%rip), %xmm0
	addq	$40, %rsp
	ret
.Ltmp13:
	.size	main, .Ltmp13-main
	.cfi_endproc

	.type	.L.fmti,@object         # @.fmti
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	 "%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
