;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (arch wasm)
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
    )

;;reg
(define reg0 'eax) ;for object ptr
(define reg1 'ebx) ;
(define reg2 'ecx)
(define reg3 'edx)
(define reg4 'esi) ;for alloc base
(define reg5 'edi)
(define reg6 'ebp)
(define reg7 'esp)

(define r0 'ax)
(define r0l 'al)
(define r0h 'ah)

(define r1l 'bl)
(define r1h 'bh)

(define regs (list reg0  reg1 reg2 reg3 reg4 reg5 reg6 reg7 r0 r0l r0h r1l r1h))
(define regs-map (list 'reg0 reg0  'reg1 reg1 'reg2 reg2 'reg3 reg3 'reg4 reg4 'reg5 reg5 'reg6 reg6 'reg7 reg7  'r0 r0 'r0l r0l 'r0h r0h ))


(define arch-bits 32)

(define all-data (make-hashtable equal-hash equal?))
(define data-offset 0)

(define (get-data var )
  (let ((ret (hashtable-ref all-data var '())))
    (if (null? ret)
      (begin (set-data var)
      (- data-offset 1))
      ret
    ))
)

(define (set-data var)
  (hashtable-set! all-data var data-offset)
  (set! data-offset (+ data-offset 1))
)


(define (asm-compile-exp exp name)
  (let ((asm
        (case (machine-type)
          ((arm32le) "")
          ((a6nt i3nt ta6nt ti3nt)  (format ""  name  name name))
          ((a6osx i3osx ta6osx ti3osx)   (format "wat2wasm ~a.s -o ~a.wasm"  name  name))
          ((a6le i3le ta6le ti3le) (format ""  name  name name))))
      )
      (printf "~a\n" asm)
      (system asm)
  )
)

(define (stext arg)
  (if (equal? arg 'start)
    (begin 
      (asm "(module")
      (asm "(table 0 anyfunc)")
      (asm "(memory $0 1)")
      (map (lambda (x)
        (asm "(global $~a (mut i32) (i32.const 0))" x) ) regs)
      ;;(asm "(import \"env\" \"consoleLog\" (func $consoleLog (param i32)))")
      (asm "(func $start (param i32) (param i32) (result i32)")
    )
    (begin 
      (sexit 0)
      (asm "(return ~a) )" (operands-rep reg0) )
    )
  )
)

(define (sexit code)
  (note "call exit 0")

)

(define (sdata arg)
    (if (equal? 'start arg)
      (begin 
        (note "section .data")
        (gen-define)
    )
    (asm ")")
    )
)

(define (gen-define)
  ;;(asm "section .data")
  (let-values ([(keyvec valvec) (hashtable-entries (get-asm-data-define))])
      (vector-for-each
        (lambda (key val)
            (data key val))
        keyvec valvec))
)


;;asm code here
(define (operands-rep x)
  (cond
      [(integer? x) 
        (format "(i32.const ~a)" (get-data x)) ]
      [(memq x regs)
        (format "(global.get $~a)" x)
      ]
      [(memq x regs-map)
        ; (printf "(memq x regs)=>~a ===========> ~a\n" (memq x regs-map) (cadr (memq x regs-map)))
        (format "(global.get $~a)"  (cadr (memq x regs-map)))
      ]
      [(string? x)
        (format "(i32.const ~a)" (get-data x))
        ]
      [(symbol? x)
        (format "(i32.const ~a)" (get-data x))
        ]
      ; [(list? x)
      ;   (let loop ((e x) (s "["))
      ;     (if (pair? e)
      ;       (begin 
      ;         (loop (cdr e) (string-append  s  (format "~a + " (car e) )))
      ;       )
      ;       (string-append s "0]")
      ;     )
      ;   )
      ; ]
      [else 
        (note "operands-rep else ~a" x)
        (format "~a" x ) ]     
  )
)

(define (data var val)
  (note "data var=~a val=~a" var val)
  (let ((offset (get-data var)))
    (cond 
      [(string? val)
        (asm "(data (i32.const ~a) \"~a\")" offset val)]
      [(number? val)
        (asm "(data (i32.const ~a) \"\\~x\" )" offset val)]
      [else
        (asm "(data (i32.const ~a) \"~a\")" offset val)]
    ))
  
  ;(asm "(data (;0;) (i32.const 1024) \"~a\" )" val)
)

(define (local index)
  (format "(local.get ~a)" index)
)

(define (fcall l . args)
  (note "fcall")
)

(define (ccall l . args)
  (note "ccall")
)

(define (gen-args args)
 (let loop [(i args) (ret "")]
            (if (pair? i)
              (begin    
                (loop (cdr i) (string-append ret (format "~a " (operands-rep (car i))) ))
              )
              ret
            )
        )
)

(define (call l . args)
    (asm "(call $~a ~a )" (symbol->asm-id  l ) (gen-args args) )
  )

(define (jmp l)
  (note "jmp")
  (asm "br $~a" (symbol->asm-id l))
  
  )

(define (cmp-jmp val1 val2 l1 l2)
  (note "cmp-jmp")
    ; ;;param eax
    ; (asm "cmp ~a,~a" (operands-rep val1)  (operands-rep val2))
    ; (if (not (null? l1) )
    ;   (asm "je ~a" (symbol->asm-id l1) )) ;; goto equal
    ; ; (printf "===========>~a\n" (symbol? l2))
    ; (if (not (null? l2) )
    ;   (asm "jne ~a" (symbol->asm-id l2) )) ;; goto not equal
)


(define (cmp type a b)
  (note "cmp")
)

;; set symbol? [a], string a ,reg 
;; set reg,reg mem,reg reg,mem
(define set
  (case-lambda 
    [(a b) 
      (unless (equal? a b) 
       (begin 
        (note "set ~a ~a (list? a)=~a number b?=~a" a b (list? a) (number? b))
        (asm "(i32.store ~a ~a)" (operands-rep a) (operands-rep b))
        ))
      ]
    [(a b c)
      (note "set")
    ]))

;;ref reg,[reg] reg,[mem]
(define mref
  (case-lambda 
    [(a b) 
      
      (note "mref a b")]
    [(a b c)
      (note "mref a b c")]))

;;set [reg],reg [mem],reg
(define mset
  (case-lambda 
    [(a b) 
      
      (note "mset a b")]
    [(a b c)
      (note "mset a b c")]))



(define (sub a b)
  (asm "(global.set $~a (i32.sub ~a ~a))" reg0 (operands-rep a) (operands-rep b) )
  )

(define (add a b)
  (asm "(global.set $~a (i32.add ~a ~a))" reg0 (operands-rep a) (operands-rep b) )
  )

(define (mul a b)
  (asm "(global.set $~a (i32.mul ~a ~a))" reg0 (operands-rep a) (operands-rep b) )
  )

(define (div a b)
  (asm "(global.set $~a (i32.div ~a ~a))" reg0 (operands-rep a) (operands-rep b) )
  )


(define (gen-params args)
 (let loop [(i args) (ret "(param ")]
            (if (pair? i)
              (begin    
                  
                (loop (cdr i) (string-append ret "i32 ") )
              )
              (string-append ret ") ")
            )
        )
)

(define (proc l args)
  (note "\n")
  (note "proc ~a ~a" l args)
  (asm "(func $~a ~a (result i32)" (symbol->asm-id l) (gen-params args) )
  ;;(label l)
  
)

(define (ret)
  (note "ret")
  (asm "(return ~a)" (operands-rep reg0))
  (asm ")")
   )

(define (label l)
  (asm "(block $~a)" (symbol->asm-id  l ))
  )

(define (sar a b)
  (asm "(global.set $~a (i32.shr_s ~a ~a))" reg0 (operands-rep a) (operands-rep b) )
)

(define (sal a b)
  (asm "(global.set $~a (i32.shl ~a ~a))" reg0 (operands-rep a) (operands-rep b) ))

(define (shl a b)
  (asm "(global.set $~a (i32.shl ~a ~a))" reg0 (operands-rep a) (operands-rep b) ))

(define (shr a b)
  (asm "(global.set $~a (i32.shr_u ~a ~a))" reg0 (operands-rep a) (operands-rep b) ))

(define (land a b)
  (asm "(global.set $~a (i32.and ~a ~a))" reg0 (operands-rep a) (operands-rep b) ))

(define (xor a b)
  (asm "(global.set $~a (i32.xor ~a ~a))" reg0 (operands-rep a) (operands-rep b) ))


(define (save a)
  (note "push ~a" (operands-rep a)))

(define (restore a)
  (note "pop ~a" (operands-rep a)))

(define (nop)
  (asm "(nop)"))



)