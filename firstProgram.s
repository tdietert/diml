	.file	"dimlProgram"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:
	leaq	-24(%rsp), %rax
	ret
.Ltmp0:
	.size	main, .Ltmp0-main

	.globl	printInt
	.align	16, 0x90
	.type	printInt,@function
printInt:
	movq	%rdi, %rcx
	movl	$.L.fmti, %edi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	jmp	printf
.Ltmp1:
	.size	printInt, .Ltmp1-printInt

	.type	.L.fmti,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.fmti:
	.asciz	"%d"
	.size	.L.fmti, 3


	.section	".note.GNU-stack","",@progbits
