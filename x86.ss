;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (x86)
  (export 
    reg0 reg1 reg2 reg3 reg4 reg5 reg6 reg7 regs regs-map

    asm set mref mset note
    add label sar sal mul sub div
    shl shr ret
    call jmp cmp-jmp cmp
    land xor save restore
    nop local proc

    fcall ccall
    stext sexit
    asm-compile-exp
    data sdata
    arch-bits
  )

(import 
    (rename (scheme) (div div2) )
    (common)
    (trace)
    (type)
    )

(define arch-bits 32)
;;reg
(define reg0 'eax) ;for object ptr
(define reg1 'ebx) ;
(define reg2 'ecx)
(define reg3 'edx)
(define reg4 'esi) ;for alloc base
(define reg5 'edi)
(define reg6 'ebp)
(define reg7 'esp)

(define regs (list reg0  reg1 reg2 reg3 reg4 reg5 reg6 reg7))
(define regs-map (list 'reg0 reg0  'reg1 reg1 'reg2 reg2 'reg3 reg3 'reg4 reg4 'reg5 reg5 'reg6 reg6 'reg7 reg7))


;;opt lib
(define (copy-string ecx esi edi)
          (asm "push ecx")
          (asm "push esi")
          (asm "push edi")
          (asm "cld")
          (asm "rep movsb")
          (asm "pop ecx")
          (asm "pop esi")
          (asm "pop edi")
          (ret)
)

(define (gen-define)
  ;;(asm "section .data")
  (let-values ([(keyvec valvec) (hashtable-entries (get-asm-data-define))])
      (vector-for-each
        (lambda (key val)
            (data key val))
        keyvec valvec))
)

(define (data var val)
  (note "data var=~a val=~a" var val)
  (cond 
    [(string? val)
      (asm  "~a db \"~a\",0 " (symbol->asm-id var) val)]
    [(number? val)
      (asm  "~a dd ~a" var val)]
    [else
      (asm  "~a dd ~a" (symbol->asm-id var) val)]
  )
)

(define (asm-compile-exp exp name)
  (let ((asm (format "`which  nasm` ~a.s -f macho && ld -macosx_version_min 10.6 -arch i386 -e _start -no_pie -lc ~a.o -o ~a" name name name)))
      ;;(printf "~a\n" asm)
      (system asm)
  )
)

;;asm code here
(define (operands-rep x)
  (cond
      [(integer? x) x]
      [(memq x regs)
        x
      ]
      [(memq x regs-map)
        ; (printf "(memq x regs)=>~a ===========> ~a\n" (memq x regs-map) (cadr (memq x regs-map)))
       (cadr (memq x regs-map))]
      [(symbol? x)
        (let ((s (symbol->string x)))
          (format "[~a]" (symbol->asm-id s))
        )]
      [(list? x)
        (let loop ((e x) (s "["))
          (if (pair? e)
            (begin 
              (loop (cdr e) (string-append  s  (format "~a + " (car e) )))
            )
            (string-append s "0]")
          )
        )
      ]
      [else 
        (note "operands-rep else ~a" x)
        (format "~a" x ) ]     
  )
)

(define (arg . args)
  (if (> (length args) 0)
      (let l ((arg  args) (i 0)) 
          (if (pair? arg)
              (begin
                (cond
                  [(number? (car arg))
                    (note "number arg=~a" (car arg))
                    (asm "mov dword [esp+ ~a], ~a" (* i 4) (car arg))
                  ]
                  [(list? (car arg)) 
                    (note "list arg=~a" (car arg))
                    (asm "mov ~a,~a" reg0 (operands-rep (car arg)) )
                    (asm "mov dword [esp+ ~a], ~a" (* i 4) reg0)
                  ]
                  [(string? (car arg))
                    (note "string arg=~a" (car arg))
                    (asm "mov dword [esp+ ~a], ~a"  (* i 4) (symbol->asm-id (car arg)))
                  ]
                  [(memq (car arg) regs-map) 
                    (note "reg arg=~a" (car arg))
                    (asm "mov dword [esp+ ~a], ~a" (* i 4) (operands-rep (car arg)) )
                  ]
                  [else
                    (note "else arg=~a" (car arg))
                    (asm "mov ~a,~a" reg0 (operands-rep (car arg)) )
                    (asm "mov dword [esp+ ~a],~a" (* i 4) reg0)
                  ]
              )
              (l (cdr arg)  (+ i 1)))
          )
      )
  )
)

(define (carg . args)
  (if (> (length args) 0)
      (let l ((arg  args) (i 0)) 
          (if (pair? arg)
              (begin
                (cond
                  [(number? (car arg))
                    (note "number arg=~a" (car arg))
                    (asm "mov ~a,~a" reg0 (car arg))
                    (sar reg0 type-shift)
                    (asm "mov dword [esp+ ~a], ~a" (* i 4) reg0 )
                  ]
                  [(list? (car arg)) 
                    (note "list arg=~a" (car arg))
                    (asm "mov ~a,~a" reg0 (operands-rep (car arg)) )
                    (sar reg0 type-shift)
                    (asm "mov dword [esp+ ~a], ~a" (* i 4) reg0)
                  ]
                  [(string? (car arg))
                    (note "string arg=~a" (car arg))
                    (asm "mov ~a,~a" reg0 (symbol->asm-id (car arg)) )
                    (sar reg0 type-shift)
                    (asm "mov dword [esp+ ~a], ~a"  (* i 4) reg0 )
                  ]
                  [(memq (car arg) regs-map) 
                    (note "reg arg=~a" (car arg))
                    (asm "mov ~a,~a" reg0 (operands-rep (car arg)) )
                    (sar reg0 type-shift)
                    (asm "mov dword [esp+ ~a], ~a" (* i 4) reg0 )
                  ]
                  [else
                    (note "else arg=~a" (car arg))
                    (asm "mov ~a,~a" reg0 (operands-rep (car arg)) )
                    (sar reg0 type-shift)
                    (asm "mov dword [esp+ ~a],~a" (* i 4) reg0)
                  ]
              )
              (l (cdr arg)  (+ i 1)))
          )
      )
  )
)

(define cmp->inst
    (lambda (op)
      (case op
        [(>) 'jg]
        [(<) 'jl]
        [(>=) 'jae]
        [(<=) 'jle]
        [(=) 'je]
        [else (printf "erro binop ~s" op)])))
        
(define (emit-cmp binop v1 v2)   
      (let* ((l1 (gen-sym (cmp->inst binop) ))
            (l2 (symbol-append l1 'end)) )
          ; (xor reg0 reg0)
          (set reg0 v1)
          ; (set reg1 v2)
          ; (sar reg0 type-shift)
          ; (sar reg1 type-shift)
          (asm "cmp ~a,~a" reg0 v2)
          (asm "~a ~a" (cmp->inst binop) l1)
          (set reg0 'false-rep)
          (jmp l2)
          (label l1)
          (set reg0 'true-rep)
          (label l2)
          (note binop)
      )
)

(define (stext)
  (asm "extern _printf")
  (asm "extern _exit")
  (asm "extern _malloc")
  (asm "")
  (asm "section .text")
  (asm "global _start")

  (asm "%macro cproc 1
    push ebx
    mov ebx, esp        ; remember current esp
    and esp, 0xFFFFFFF0 ; align to next 16 byte boundary (could be zero offset!)
    sub esp, 12         ; skip ahead 12 so we can store original esp
    push ebx            ; store esp (16 bytes aligned again)
    sub esp, %1         ; pad for arguments (make conditional?)
%endmacro

; arg must match most recent call to clib_prolog
%macro ceproc 1
    add esp, %1         ; remove arg padding
    pop ebx             ; get original esp
    mov esp, ebx        ; restore
    pop ebx
%endmacro
")

  (asm "_start:")
  (asm "push ebp")
  (asm "mov	ebp, esp")
  (asm "and esp,0xFFFFFFF0")
  (asm "sub esp,12")

  ; (asm "mov eax ,dword [ebp+4 ]")
  ; (asm "mov eax ,dword [ebp+0 ]")
  ; (asm "pop eax  ;;argc to eax")
  ; (asm "pop ebx  ;;argv to ebx")
  ; (asm "mov ebp,esp")
  ; (asm "and esp,0xFFFFFFF0")
  ; (asm "sub esp,16")
)

(define (sexit code)
  (note "call exit 0")
  (fcall 'exit 0)
  (asm "pop ebp")
  (asm "ret")
)
(define (sdata)
    (asm "section .data")
    (gen-define)
)

(define (local index)
  (if (number? index)
    (list 'ebp (* 4 index))
    index
  )
)

(define (align args)
  (* 16 (flonum->fixnum (+ 0.9 (/ (* 4 (length args)) 16))))
)

(define (ccall l . args)
  (asm "cproc ~a" (align args))
  ; (if (> (length args) 0)
  ;   (begin 
  ;     (asm "and esp,0xFFFFFFF0")
  ;     (asm "sub  esp,~a" (align args))
  ;    ))
  (apply carg args)
  (asm "call _~a" (symbol->asm-id l))
  ; (if (> (length args) 0)
  ;   (asm "add esp, ~a" (align args) )
  ;   )
  (asm "ceproc ~a" (align args))
  )

(define (fcall l . args)
  (asm "cproc ~a" (align args))
  ; (if (> (length args) 0)
  ;   (begin 
  ;     (asm "and esp,0xFFFFFFF0")
  ;     (asm "sub  esp,~a" (align args))
  ;    ))
  (apply arg args)
  (asm "call _~a" (symbol->asm-id l))
  ; (if (> (length args) 0)
  ;   (asm "add esp, ~a" (align args) )
  ;   )
  (asm "ceproc ~a" (align args))
  )

(define (call l . args)
  (if (> (length args) 0)
    (begin 
      (asm "sub  esp,~a" (align args))
     ))
  (apply arg args)
  (asm "call ~a" (symbol->asm-id l))
  (if (> (length args) 0)
    (asm "add esp, ~a" (align args) )
    )
  )

(define (jmp l)
  (asm "jmp ~a" (symbol->asm-id l)))

(define (cmp-jmp val1 val2 l1 l2)
    ;;param eax
    (asm "cmp ~a,~a" (operands-rep val1)  (operands-rep val2))
    (if (not (null? l1) )
      (asm "je ~a" (symbol->asm-id l1) )) ;; goto equal
    ; (printf "===========>~a\n" (symbol? l2))
    (if (not (null? l2) )
      (asm "jne ~a" (symbol->asm-id l2) )) ;; goto not equal
)

(define (cmp type a b)
  (emit-cmp type a b)
)

;; set symbol? [a], string a ,reg 
;; set reg,reg mem,reg reg,mem
(define set
  (case-lambda 
    [(a b) 
      (note "set ~a ~a (list? a)=~a" a b (list? a))
      (unless (equal? a b)
        (begin 
          (if (and (list? a) (or (list? b) (symbol? b)))
            (begin 
              (asm "mov ~a,~a" reg0 (operands-rep b) )
              (asm "mov ~a,~a" (operands-rep a) reg0 ))
            (asm "mov ~a~a,~a" (if (list? a) "dword " "") (operands-rep a) (operands-rep b) ))
          )
          
        )]
    [(a b c)
    (unless (equal? a b)
      (asm "mov ~a,~a ;;~a" (operands-rep a) (operands-rep b) c ))]))

;;ref reg,[reg] reg,[mem]
(define mref
  (case-lambda 
    [(a b) 
      (unless (equal? a b)
        (asm "mov ~a,[~a]" (operands-rep a) (operands-rep b) ))]
    [(a b c)
    (unless (equal? a b)
      (asm "mov ~a,[~a] ;;~a" (operands-rep a) (operands-rep b) c ))]))

;;set [reg],reg [mem],reg
(define mset
  (case-lambda 
    [(a b) 
      (unless (equal? a b)
        (asm "mov [~a],~a" (operands-rep a) (operands-rep b) ))]
    [(a b c)
    (unless (equal? a b)
      (asm "mov [~a],~a ;;~a" (operands-rep a) (operands-rep b) c ))]))


(define (add a b)
  (asm "add ~a,~a" (operands-rep a) (operands-rep b))
  )

(define (sub a b)
  (if (or (number? a) (list? a))
    (begin 
      (asm "mov ~a,~a" reg0 (operands-rep a))
      (asm "sub ~a,~a" reg0 (operands-rep b))
    )
    (asm "sub ~a,~a" (operands-rep a) (operands-rep b))
  )
  
  )

(define (mul a b)
  (asm "xor edx,edx")
  (asm "mov ~a,~a" reg0 (operands-rep a))
  (asm "mov ~a,~a" reg2 (operands-rep b))
  (asm "mul ~a"  reg2)
  )

(define (div a b)
  (asm "xor edx,edx")
  (asm "mov ~a,~a" reg0 (operands-rep a))
  (asm "mov ~a,~a" reg2 (operands-rep b))
  (asm "div ~a"  reg2)
  )

(define (proc l)
  (note "\n")
  (note "proc ~a" l)
  (label l)
  (asm "push ebp")
  (asm "mov	ebp, esp")
  (asm "add ebp, 8")
)

(define (ret)
  (asm "pop ebp")
  (asm "ret"))

(define (label l)
  (asm "~a:" (symbol->asm-id  l ))
  )

(define (sar a b)
  (asm "sar ~a,~a" (operands-rep a) (operands-rep b) ))

(define (sal a b)
  (asm "sal ~a,~a" a b))

(define (shl a b)
  (asm "shl ~a,~a" a b))

(define (shr a b)
  (asm "shr ~a,~a" a b))

(define (land a b)
  (asm "and ~a,~a" (operands-rep a) (operands-rep b) ))

(define (xor a b)
  (asm "xor ~a,~a" a b))

(define (save a)
  (asm "push ~a" (operands-rep a)))

(define (restore a)
  (asm "pop ~a" (operands-rep a)))

(define (nop)
  (asm "nop"))


;;instruct start here

; (define (saves)
;   ; (asm "pusha")
;   (save reg1)
;   (save reg2)
;   (save reg3)
;   (save reg4)
; )

; (define (restores)
;   (restore reg4)
;   (restore reg3)
;   (restore reg2)
;   (restore reg1)
; )


)