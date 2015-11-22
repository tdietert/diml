	.file	"firstProgram.ll"
	.text
	.align	16, 0x90
	.type	f,@function
f:                                      # @f
# BB#0:                                 # %entry
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
	subq	$56, %rsp
	vucomisd	.LCPI2_0(%rip), %xmm0
	jae	.LBB2_1
# BB#2:                                 # %if.exit
	vmovsd	.LCPI2_3(%rip), %xmm0
	addq	$56, %rsp
	ret
.LBB2_1:                                # %if.else
	vmovsd	%xmm0, 8(%rsp)          # 8-byte Spill
	vaddsd	.LCPI2_1(%rip), %xmm0, %xmm0
	vmovsd	%xmm1, 40(%rsp)         # 8-byte Spill
	vmovaps	%xmm2, 16(%rsp)         # 16-byte Spill
	callq	fib
	vmovsd	%xmm0, 48(%rsp)         # 8-byte Spill
	vmovsd	8(%rsp), %xmm0          # 8-byte Reload
	vaddsd	.LCPI2_2(%rip), %xmm0, %xmm0
	vmovsd	40(%rsp), %xmm1         # 8-byte Reload
	vmovaps	16(%rsp), %xmm2         # 16-byte Reload
	callq	fib
	vmovsd	48(%rsp), %xmm1         # 8-byte Reload
	vaddsd	%xmm0, %xmm1, %xmm0
	addq	$56, %rsp
	ret
.Ltmp2:
	.size	fib, .Ltmp2-fib

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI3_0:
	.quad	4681608360884174848     # double 1.0E+5
	.text
	.globl	fst
	.align	16, 0x90
	.type	fst,@function
fst:                                    # @fst
# BB#0:                                 # %entry
	vmovsd	.LCPI3_0(%rip), %xmm0
	ret
.Ltmp3:
	.size	fst, .Ltmp3-fst

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI4_0:
	.quad	4621819117588971520     # double 10
.LCPI4_1:
	.quad	4687288231794835456     # double 234234
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
# BB#0:                                 # %entry
	pushq	%rax
	vmovsd	.LCPI4_0(%rip), %xmm0
	vmovsd	.LCPI4_1(%rip), %xmm1
	callq	fib
	vmovsd	%xmm0, (%rsp)           # 8-byte Spill
	vmovsd	.LCPI4_1(%rip), %xmm0
	callq	f
	vmovsd	(%rsp), %xmm1           # 8-byte Reload
	vaddsd	%xmm0, %xmm1, %xmm0
	popq	%rax
	ret
.Ltmp4:
	.size	main, .Ltmp4-main

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
