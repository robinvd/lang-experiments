	.text
	.file	"test.c"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	pushq	%rbx
.Lcfi0:
	.cfi_def_cfa_offset 16
.Lcfi1:
	.cfi_offset %rbx, -16
	movl	$8, %edi
	callq	alloc
	movq	%rax, %rbx
	movl	$5, (%rbx)
	movl	$1, %edi
	movl	$.L.str, %esi
	movl	$5, %edx
	xorl	%eax, %eax
	callq	__printf_chk
	xorl	%eax, %eax
	callq	enterGC
	movl	(%rbx), %edx
	movl	$1, %edi
	movl	$.L.str, %esi
	xorl	%eax, %eax
	callq	__printf_chk
	xorl	%eax, %eax
	popq	%rbx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%d\n"
	.size	.L.str, 4


	.ident	"clang version 5.0.0 (tags/RELEASE_500/final)"
	.section	".note.GNU-stack","",@progbits
