





































































	.text

	.align	16, 0x90
	.globl	__gmpn_divexact_1
	.type	__gmpn_divexact_1,@function
	
__gmpn_divexact_1:






	movl	16(%esp), %eax
	subl	$20, %esp	

	movl	%esi, 12(%esp)
	movl	28(%esp), %esi

	movl	%ebx, 16(%esp)
	movl	32(%esp), %ebx

	bsfl	%eax, %ecx		

	movl	%ebp, 4(%esp)

	shrl	%cl, %eax		

	movl	%eax, %edx
	shrl	%eax			

	movl	%edx, 36(%esp)
	andl	$127, %eax


	


        call    .Lmovl_eip_ebp
        addl    $_GLOBAL_OFFSET_TABLE_, %ebp
        movl    __gmp_binvert_limb_table@GOT(%ebp), %ebp

	movzbl	(%eax,%ebp), %ebp		


	leal	(%ebp,%ebp), %eax	

	imull	%ebp, %ebp		

	movl	%edi, 8(%esp)
	movl	24(%esp), %edi

	leal	(%esi,%ebx,4), %esi	

	imull	36(%esp), %ebp	

	subl	%ebp, %eax		
	leal	(%eax,%eax), %ebp	

	imull	%eax, %eax		

	leal	(%edi,%ebx,4), %edi	
	negl	%ebx			

	movl	%edi, 24(%esp)

	imull	36(%esp), %eax	

	subl	%eax, %ebp		

	

	movl	%ebp, (%esp)
	movl	(%esi,%ebx,4), %eax	

	orl	%ecx, %ecx
	jnz	.Leven

	
	jmp	.Lodd_entry













.Lodd_top:
	
	
	
	
	
	
	

	mull	36(%esp)

	movl	(%esi,%ebx,4), %eax
	subl	%ecx, %eax

	sbbl	%ecx, %ecx
	subl	%edx, %eax

	sbbl	$0, %ecx

.Lodd_entry:
	imull	(%esp), %eax

	movl	%eax, (%edi,%ebx,4)
	negl	%ecx

	incl	%ebx
	jnz	.Lodd_top


	movl	12(%esp), %esi

	movl	8(%esp), %edi

	movl	4(%esp), %ebp

	movl	16(%esp), %ebx
	addl	$20, %esp

	ret




.Leven:
	
	
	
	
	
	
	

	xorl	%ebp, %ebp		
	xorl	%edx, %edx		

	incl	%ebx
	jz	.Leven_one

	movl	(%esi,%ebx,4), %edi	

	shrdl	%cl, %edi, %eax

	jmp	.Leven_entry


.Leven_top:
	
	
	
	
	
	
	

	movl	(%esi,%ebx,4), %edi

	mull	36(%esp)

	movl	-4(%esi,%ebx,4), %eax
	shrdl	%cl, %edi, %eax

	subl	%ebp, %eax

	sbbl	%ebp, %ebp
	subl	%edx, %eax

	sbbl	$0, %ebp

.Leven_entry:
	imull	(%esp), %eax

	movl	24(%esp), %edi
	negl	%ebp

	movl	%eax, -4(%edi,%ebx,4)
	incl	%ebx
	jnz	.Leven_top



	mull	36(%esp)

	movl	-4(%esi), %eax

.Leven_one:
	shrl	%cl, %eax
	movl	12(%esp), %esi

	subl	%ebp, %eax
	movl	4(%esp), %ebp

	subl	%edx, %eax
	movl	16(%esp), %ebx

	imull	(%esp), %eax

	movl	%eax, -4(%edi)
	movl	8(%esp), %edi
	addl	$20, %esp

	ret




.Lmovl_eip_ebp:
	movl	(%esp), %ebp
	ret
	.size	__gmpn_divexact_1,.-__gmpn_divexact_1
