;;/opt/local/bin/nasm test-print.s -f macho && ld -macosx_version_min 10.6 -arch i386 -e _start -no_pie -lc  test-print.o test-print
extern _printf
extern _exit
extern _malloc

section .text
global _start

%macro clib_prolog 1
    mov ebx, esp        ; remember current esp
    and esp, 0xFFFFFFF0 ; align to next 16 byte boundary (could be zero offset!)
    sub esp, 12         ; skip ahead 12 so we can store original esp
    push ebx            ; store esp (16 bytes aligned again)
    sub esp, %1         ; pad for arguments (make conditional?)
%endmacro

; arg must match most recent call to clib_prolog
%macro clib_epilog 1
    add esp, %1         ; remove arg padding
    pop ebx             ; get original esp
    mov esp, ebx        ; restore
%endmacro

_start:
; Store 'argc' into EAX
    pop     eax
    ; Store 'argv' into EBX
    pop     ebx

    ; Align stack on a 16 bytes boundary,
    ; as we'll use C library functions
    mov     ebp,                esp
    and     esp,                0xFFFFFFF0
   
    ; Stack space for local variables
    ; A little more space than needed, but that will
    ; ensure the stack is still aligned
    
    call test_print

    ;sub     esp,16
    ; Call 'printf': printf( hello, ebx, eax );
    ;mov     dword[ esp ],       hello
    ;mov     dword[ esp + 4 ],   ebx
    ;mov     dword[ esp + 8 ],   eax
    ;call   _printf
    ;add esp,16


    ; Call 'exit': exit( 0 );
    mov     dword[ esp ],       0
    call   _exit
    ret

test_print:
    
    clib_prolog 16
    mov     dword[ esp ],       hello
    mov     dword[ esp + 4 ],   100
    call   _printf
    clib_epilog 16

    ret


section .data
hello db "Program name: %d", 10, 0