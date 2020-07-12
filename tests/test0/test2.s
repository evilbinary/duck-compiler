	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 12
	.intel_syntax noprefix
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
## BB#0:
	push	ebp
	mov	ebp, esp
	sub	esp, 24
	mov	eax, 8
	mov	dword ptr [ebp - 4], 0
	mov	dword ptr [esp], 8
	mov	dword ptr [ebp - 12], eax ## 4-byte Spill
	call	_malloc
	xor	ecx, ecx
	mov	dword ptr [ebp - 8], eax
	mov	eax, ecx
	add	esp, 24
	pop	ebp
	ret


.subsections_via_symbols
