	.file	"fp.bc"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4622382067542392832     # double 11
	.text
	.align	16, 0x90
	.type	f,@function
f:                                      # @f
# BB#0:                                 # %entry
	vmovaps	%xmm1, %xmm2
	vmovaps	%xmm0, %xmm1
	vmovsd	.LCPI0_0(%rip), %xmm0
	jmp	lambda                  # TAILCALL
.Ltmp0:
	.size	f, .Ltmp0-f

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI1_0:
	.quad	4611686018427387904     # double 2
	.text
	.align	16, 0x90
	.type	lambda,@function
lambda:                                 # @lambda
# BB#0:                                 # %entry
	vaddsd	.LCPI1_0(%rip), %xmm0, %xmm0
	ret
.Ltmp1:
	.size	lambda, .Ltmp1-lambda

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4611686018427387904     # double 2
.LCPI2_1:
	.quad	-4616189618054758400    # double -1
.LCPI2_2:
	.quad	-4611686018427387904    # double -2
.LCPI2_3:
	.quad	4607182418800017408     # double 1
	.text
	.align	16, 0x90
	.type	fib,@function
fib:                                    # @fib
# BB#0:                                 # %entry
	subq	$40, %rsp
	vucomisd	.LCPI2_0(%rip), %xmm0
	jae	.LBB2_1
# BB#2:                                 # %if.exit
	vmovsd	.LCPI2_3(%rip), %xmm0
	addq	$40, %rsp
	ret
.LBB2_1:                                # %if.else
	vmovsd	%xmm0, 8(%rsp)          # 8-byte Spill
	vaddsd	.LCPI2_1(%rip), %xmm0, %xmm0
	vmovsd	%xmm1, 24(%rsp)         # 8-byte Spill
	vmovsd	%xmm2, 16(%rsp)         # 8-byte Spill
	callq	fib
	vmovsd	%xmm0, 32(%rsp)         # 8-byte Spill
	vmovsd	8(%rsp), %xmm0          # 8-byte Reload
	vaddsd	.LCPI2_2(%rip), %xmm0, %xmm0
	vmovsd	24(%rsp), %xmm1         # 8-byte Reload
	vmovsd	16(%rsp), %xmm2         # 8-byte Reload
	callq	fib
	vmovsd	32(%rsp), %xmm1         # 8-byte Reload
	vaddsd	%xmm0, %xmm1, %xmm0
	addq	$40, %rsp
	ret
.Ltmp2:
	.size	fib, .Ltmp2-fib

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI3_0:
	.quad	4621819117588971520     # double 10
.LCPI3_1:
	.quad	4607182418800017408     # double 1
.LCPI3_2:
	.quad	4611686018427387904     # double 2
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	vmovsd	.LCPI3_0(%rip), %xmm0
	vmovsd	.LCPI3_1(%rip), %xmm1
	vmovsd	.LCPI3_2(%rip), %xmm2
	callq	fib
	vmovsd	%xmm0, (%rsp)           # 8-byte Spill
	vmovsd	.LCPI3_1(%rip), %xmm0
	vmovsd	.LCPI3_2(%rip), %xmm1
	callq	f
	vmovsd	(%rsp), %xmm1           # 8-byte Reload
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
fst:                                    # @fst
	.cfi_startproc
# BB#0:                                 # %entry
	vmovapd	%xmm0, -24(%rsp)
	vmovlpd	%xmm0, -8(%rsp)
	ret
.Ltmp6:
	.size	fst, .Ltmp6-fst
	.cfi_endproc

	.globl	snd
	.align	16, 0x90
	.type	snd,@function
snd:                                    # @snd
	.cfi_startproc
# BB#0:                                 # %entry
	vmovapd	%xmm0, -24(%rsp)
	vmovhpd	%xmm0, -8(%rsp)
	vunpckhpd	%xmm0, %xmm0, %xmm0 # xmm0 = xmm0[1,1]
	ret
.Ltmp7:
	.size	snd, .Ltmp7-snd
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

	.type	.L.fmti,@object         # @.fmti
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	 "%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
