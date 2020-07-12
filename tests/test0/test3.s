	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 12
	.intel_syntax noprefix
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
## BB#0:
	push	ebp
	mov	ebp, esp
	sub	esp, 12
	mov	eax, dword ptr [ebp + 12]
	mov	ecx, dword ptr [ebp + 8]
	xor	edx, edx
	mov	dword ptr [ebp - 4], 0
	mov	dword ptr [ebp - 8], ecx
	mov	dword ptr [ebp - 12], eax
	mov	eax, edx
	add	esp, 12
	pop	ebp
	ret


.subsections_via_symbols
