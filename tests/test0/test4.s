	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 12
	.intel_syntax noprefix
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
## BB#0:
	push	ebp
	mov	ebp, esp
	sub	esp, 40
	call	L0$pb
L0$pb:
	pop	eax
	mov	ecx, dword ptr [ebp + 12]
	mov	edx, dword ptr [ebp + 8]
	lea	eax, [eax + L_.str-L0$pb]
	mov	dword ptr [ebp - 4], 0
	mov	dword ptr [ebp - 8], edx
	mov	dword ptr [ebp - 12], ecx
	mov	dword ptr [esp], eax
	call	_printf
	mov	ecx, 8
	mov	dword ptr [esp], 8
	mov	dword ptr [ebp - 20], eax ## 4-byte Spill
	mov	dword ptr [ebp - 24], ecx ## 4-byte Spill
	call	_malloc
	xor	ecx, ecx
	mov	dword ptr [ebp - 16], eax
	mov	eax, dword ptr [ebp - 16]
	add	eax, 1
	mov	dword ptr [ebp - 16], eax
	mov	eax, ecx
	add	esp, 40
	pop	ebp
	ret

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"hello\n"


.subsections_via_symbols
