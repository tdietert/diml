	.file	"dimlProgram"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4607182418800017408
.LCPI0_1:
	.quad	4611686018427387904
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	vmovsd	.LCPI0_0(%rip), %xmm0
	vmovsd	.LCPI0_1(%rip), %xmm1
	callq	fst
	vmovsd	%xmm0, (%rsp)
	vmovsd	.LCPI0_0(%rip), %xmm0
	vmovsd	.LCPI0_1(%rip), %xmm1
	callq	snd
	vmovsd	(%rsp), %xmm1
	vaddsd	%xmm0, %xmm1, %xmm0
	vcvttsd2si	%xmm0, %rdi
	callq	printInt
	vmovsd	.LCPI0_0(%rip), %xmm0
	popq	%rax
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.globl	fst
	.align	16, 0x90
	.type	fst,@function
fst:
	.cfi_startproc
	ret
.Ltmp3:
	.size	fst, .Ltmp3-fst
	.cfi_endproc

	.globl	snd
	.align	16, 0x90
	.type	snd,@function
snd:
	.cfi_startproc
	vmovaps	%xmm1, %xmm0
	ret
.Ltmp4:
	.size	snd, .Ltmp4-snd
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
