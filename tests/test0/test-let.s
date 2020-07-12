;;/opt/local/bin/nasm test-let.s -f macho && ld -e _start test-let.o -o test-let

section .data
a dd	10  
b dd 0
var.2 dd 0
var.3 dd 0
nil dd 0

section .text
global _start

_start:
mov eax,10
mov [a],eax
mov eax,11
mov [b],eax
mov eax,[nil]
mov [var.2],eax
;jmp var.2
mov [var.3],eax
;jmp var.3
ret


