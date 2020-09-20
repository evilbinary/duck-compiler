;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (llvm)
  (export 
    reg0 reg1 reg2 reg3 reg4 reg5 reg6 reg7 regs regs-map
    asm set mref mset note
    add label sar sal mul sub div
    shl shr ret
    call jmp cmp-jmp cmp
    land xor save restore
    nop local proc lproc pret lret

    fcall ccall
    stext sexit
    asm-compile-exp
    data sdata
    arch-bits
  )

  (import 
    (rename (scheme) (div div2) )
    (options)
    (common)
    )

;;reg
(define reg0 '%eax) ;for object ptr
(define reg1 '%ebx) ;
(define reg2 '%ecx)
(define reg3 '%edx)
(define reg4 '%esi) ;for alloc base
(define reg5 '%edi)
(define reg6 '%ebp)
(define reg7 '%esp)

(define r0 '%ax)
(define r0l '%al)
(define r0h '%ah)

(define r1l '%bl)
(define r1h '%bh)

(define regs (list reg0  reg1 reg2 reg3 reg4 reg5 reg6 reg7 ))
(define regs-map (list 'reg0 reg0  'reg1 reg1 'reg2 reg2 'reg3 reg3 'reg4 reg4 'reg5 reg5 'reg6 reg6 'reg7 reg7  'r0 r0 'r0l r0l 'r0h r0h ))


(define arch-bits 32)

(define all-data (make-hashtable equal-hash equal?))

(define (get-data var )
  (let ((val (hashtable-ref all-data var '())))
    (if (null? val)
      (asm-data-get var '())
    )
  )
)

(define (set-data var val)
  (hashtable-set! all-data var val)
)

(define symbol-prefix
  (case (machine-type)
    ((arm32le) "_")
    ((a6nt i3nt ta6nt ti3nt)  "_")
    ((a6osx i3osx ta6osx ti3osx)  "")
    ((a6le i3le ta6le ti3le) "")))


(define (asm-compile-exp exp name)
  (let ((asm
        (case (machine-type)
          ((arm32le) "")
          ((a6nt i3nt ta6nt ti3nt)  (format ""  name  name name))
          ((a6osx i3osx ta6osx ti3osx)   (format "llvm-gcc ~a~a -o ~a"  name (option-get 'obj.name ".s") name))
          ((a6le i3le ta6le ti3le) (format ""  name  name name))))
      )
      (printf "~a\n" asm)
      (system asm)
  )
)

(define (stext arg)
  (if (equal? arg 'start)
    (begin 
      (asm "; ModuleID = 'duck'")
      (asm "declare i32 @printf(i32*, ...)")
      
      (asm "define i32 @main(i32,i32 ) {")
      (map (lambda (x)
      (asm "~a = alloca i32, align 4" x)
      ;;(asm "%~a = load i32, i32* %.~a, align 4" x x)
      )  regs)
      (gen-local-params '(arg0 arg1))
    )
    (begin 
      (sexit 0)
      (asm "ret i32 0")
      (asm "}")
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
    (asm "")
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
        x ]
      [(memq x regs)
        x
      ]
      [(memq x regs-map)
        ; (printf "(memq x regs)=>~a ===========> ~a\n" (memq x regs-map) (cadr (memq x regs-map)))
        (cadr (memq x regs-map))
      ]
      [(string? x)
       x
        ]
      [(symbol? x)
        x
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
  (set-data var val)
  (cond 
    [(string? val)
      (asm "@~a = constant [~a x i8] c\"~a\", align 1" var (string-length val) val)]
    [(number? val)
      (asm "@~a = constant i32 ~a" var val)]
    [else
      (asm "@~a = global i32 \"~a\"" var val)]
  )
  
  ;(asm "(data (;0;) (i32.const 1024) \"~a\" )" val)
)

(define (local index)
  (string->symbol (format "%local.~a" index))
)

(define (fcall l . args)
  (note "fcall ~a ~a" l args)
  (let ((def ""))
    (if (equal? l 'printf)
      (set! def "(i32*, ...)")
    )
    (asm "%call.ret.~a = call i32 ~a @~a~a(~a)"  binary-offset def symbol-prefix (symbol->asm-id l)  (gen-call-args args))
    )
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


(define (gen-call-args args)
 (let loop [(i args) (ret "")]
            (if (pair? i)
              (begin
                (note "arg=~a" (car i))
                (cond 
                  [(string? (operands-rep (car i)))
                    ;;(asm "%call.arg.~a = load i8, i8* ~a, align 4" binary-offset (operands-rep (car i)) )
                    (asm "%call.arg.~a = alloca i32, align 4" binary-offset)
                    (asm "store i32 ptrtoint ([~a x i8]* @~a to i32), i32* %call.arg.~a, align 4" 
                          (string-length  (get-data (symbol->asm-id (car i)) ) )
                          (symbol->asm-id (car i)) 
                          binary-offset )
                    (set! ret (string-append ret (format "i32* %call.arg.~a," binary-offset  )))
                  ]
                  [else
                    (asm "%call.arg.~a = load i32, i32* ~a, align 4" binary-offset (operands-rep (car i)) )  
                    (set! ret (string-append ret (format "i32 %call.arg.~a," binary-offset  )))
                  ]
                )
                (set! binary-offset (+ binary-offset 1))
                (loop (cdr i) ret )
              )
              (substring ret 0 (- (string-length ret) 1) )
            )
        )
)

(define (call l . args)
    (note "call $~a ~a" (symbol->asm-id  l ) (gen-args args) )
    (let ((ret (format "%call.ret.~a"  binary-offset )))
      (asm "~a = call i32 @~a(~a)" ret (symbol->asm-id l) (gen-call-args args) )
      (asm "store i32 ~a, i32* ~a, align 4" ret reg0)
    )
  )

(define (jmp l)
  (note "jmp")
  (asm "br label %~a" (symbol->asm-id l))
  
  )

(define (cmp-jmp val1 val2 l1 l2)
  (note "cmp-jmp ~a ~a ~a ~a" val1 val2 l1 l2)
  ; ;;param eax
  (cond 
    [(and (memq (operands-rep val1) regs) (memq (operands-rep val2) regs) )
      (note "1")
      (asm "%cmp.a.~a = load i32, i32* ~a, align 4" binary-offset (operands-rep val1) )
      (asm "%cmp.b.~a = load i32, i32* ~a, align 4" binary-offset (operands-rep val2) )
      (asm "%cmp.~a = icmp eq i32 %cmp.a.~a, %cmp.b.~a" binary-offset binary-offset  )
    ]
    [(and (memq (operands-rep val1) regs) (number? (operands-rep val2)) )
      (note "2")
      (asm "%cmp.a.~a = load i32, i32* ~a, align 4" binary-offset (operands-rep val1) )
      (asm "%cmp.~a = icmp eq i32 %cmp.a.~a, ~a" binary-offset binary-offset  (operands-rep val2) )
    ]
    [(and (memq (operands-rep val1) regs) (symbol? (operands-rep val2)) )
      (note "3")
      (asm "%cmp.a.~a = load i32, i32* ~a, align 4" binary-offset (operands-rep val1) )
      (asm "%cmp.b.~a = load i32, i32* @~a, align 4" binary-offset (symbol->asm-id (operands-rep val2) ))
      (asm "%cmp.~a = icmp eq i32 %cmp.a.~a, %cmp.b.~a" binary-offset binary-offset  binary-offset )
    ]
    [else
      (note "4")
      (asm "%cmp.~a = icmp eq i32 ~a, ~a" binary-offset (operands-rep val1)  (operands-rep val2))
    ]
  )
  (cond 
    [(and (not (null? l1))  (not (null? l2)))
      (asm "br i1 %cmp.~a, label %~a, label %~a" binary-offset (symbol->asm-id l1) (symbol->asm-id l2) )
    ]
    [(and (not (null? l1))  (null? l2))
      (set! l2 (format "label.~a" binary-offset))
      (asm "br i1 %cmp.~a, label %~a, label %~a" binary-offset (symbol->asm-id l1) l2 )
      (asm "~a:" l2)
    ]
    [(and (not (null? l2))  (null? l1))
      (set! l1 (format "label.~a" binary-offset))
      (asm "br i1 %cmp.~a, label %~a, label %~a" binary-offset l1 (symbol->asm-id l2) )
      (asm "~a:" l1)
    ]
  )

  (set! binary-offset (+ binary-offset 1))
  ; (asm "cmp.~a = icmp eq ~a, nop " binary-offset (operands-rep val1)  (operands-rep val2))
  ; (if (not (null? l1) )
  ;   (asm "je ~a" (symbol->asm-id l1) )) ;; goto equal
  ; ; (printf "===========>~a\n" (symbol? l2))
  ; (if (not (null? l2) )
  ;   (asm "jne ~a" (symbol->asm-id l2) )) ;; goto not equal

)


(define (cmp type a b)
  (note "cmp")
)

(define (is-reg x)
  (or (memq x regs) (memq x regs-map) )
)

;; set symbol? [a], string a ,reg 
;; set reg,reg mem,reg reg,mem
(define set
  (case-lambda 
    [(a b) 
     (let ((type "set"))
        (set! a (operands-rep a ))
        (set! b (operands-rep b ))

        (unless (equal? a b) 
        (note "set ~a ~a (list? a)=~a  (string? a)=~a (number? b)=~a" a b (list? a) (string? a) (number? b))
        (cond
          [(and (memq a regs) (memq b regs) )
            (note "1")
            (asm "%~a.a~a = load i32, i32* ~a, align 4" type binary-offset a)
            (asm "store i32 %~a.a~a, i32* ~a, align 4" type binary-offset  b)
          ]
          [(and (string? a) (memq b regs) ) ;;str a=>b
            (note "2")
            (asm "store i32 ptrtoint ([~a x i8]* @~a to i32), i32* ~a, align 4" (string-length (get-data a)) a b)
          ]
          [(and (string? b) (memq a regs) ) ;;str b=>a
            (note "3")
            (asm "store i32 ptrtoint ([~a x i8]* @~a to i32), i32* ~a, align 4" (string-length (get-data b)) b a)
          ]
          [(and (memq a regs)  (symbol? b )) ;; local0=>reg0 b=>a
            (note "4")
            (asm "%~a.a~a = load i32, i32* ~a, align 4" type binary-offset a)
            (asm "store i32 %~a.a~a, i32* ~a, align 4" type binary-offset  b)
          ]
          [(and (memq b regs)  (symbol? a )) ;; reg b=> local a
            (note "5")
            (asm "%~a.a~a = load i32, i32* ~a, align 4" type binary-offset b)
            (asm "store i32 %~a.a~a, i32* ~a, align 4"  type  binary-offset  a)
          ]
          [(and (symbol? a) (number? b))
            (asm "store i32 ~a, i32* ~a, align 4" b a)
          ]
          [(and (symbol? b) (number? a))
            (asm "store i32 ~a, i32* ~a, align 4" a b)
          ]
          [else
            (asm "error")
          ]
        )
        (set! binary-offset (+ binary-offset 1))
        )   
      )
      ]
    [(a b c)
      (note "set")
    ])
    )

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

(define binary-offset 0)
(define (binary-op type a b )
  (note "~a ~a,~a" type a b )
  (set! a (operands-rep a ))
  (set! b (operands-rep b ))
  (cond 
    [(and (memq a regs) (memq b regs) )
        (asm "%~a.a~a = load i32, i32* ~a, align 4" type binary-offset a)
        (asm "%~a.b~a = load i32, i32* ~a, align 4" type binary-offset b)
        (asm "%~a.ret~a = ~a  i32 %~a.a~a, %~a.b~a" type binary-offset type type binary-offset type binary-offset)
        (asm "store i32 %~a.ret~a, i32* ~a, align 4" type binary-offset reg0 )
    ]
    [(and (memq a regs) (number? b) )
        (asm "%~a.a~a = load i32, i32* ~a, align 4" type binary-offset a)
        (asm "%~a.ret~a = ~a  i32 %~a.a~a, ~a" type binary-offset type type binary-offset b)
        (asm "store i32 %~a.ret~a, i32* ~a, align 4" type binary-offset reg0)
    ]
    [(and (memq b regs) (number? a) )
        (asm "%~a.b~a = load i32, i32* ~a, align 4" type binary-offset b)
        (asm "%~a.ret~a = ~a  i32 %~a.a~a, ~a" type binary-offset type type binary-offset a)
        (asm "store i32 %~a.ret~a, i32* ~a, align 4" type binary-offset reg0)
    ]
    [(and (number? a) (number? b))
      (asm "store i32 ~a, i32* ~a, align 4" (+ a b) reg0)
    ]
    [else 
      (asm "error ")
    ]
    
    )
    (set! binary-offset (+ binary-offset 1))
)

(define (sub a b)
  (binary-op 'sub a b )
  )

(define (add a b)
  (binary-op 'add a b )
  )

(define (mul a b)
  (binary-op 'mul a b )
  )

(define (div a b)
  (binary-op 'div a b )
  )


(define (gen-params args)
 (let loop [(i args) (ret " ")]
            (if (pair? i)
              (begin
                (loop (cdr i) (string-append ret (format "i32 ," ))  )
              )
              (substring ret 0 (- (string-length ret) 1) )
            )
        )
)

(define (gen-local-params args)
 (let loop [(i args) (offset 0)]
            (if (pair? i)
              (begin
                (asm "%local.~a = alloca i32, align 4" offset)
                (asm "store i32 %~a, i32* %local.~a, align 4" offset offset)
                (loop (cdr i) (+ offset 1))
              )
            )
        )
)

(define (proc l args)
  (set! binary-offset 0)
  (note "\n")
  (note "proc ~a ~a" l args)
  (asm "define i32 @~a(~a) { " (symbol->asm-id l) (gen-params args) )
  (map (lambda (x)
    (asm "~a = alloca i32, align 4" x)
    ;;(asm "%~a = load i32, i32* %.~a, align 4" x x)
    )  regs)
  (gen-local-params args)
  
  
)

(define (lproc l args)
  (proc l args)
)

(define (pret)
  (ret)
   )

(define (lret)
  (ret)
   )

(define (ret)
  (note "ret")
  ;;(asm "ret i32 0" (operands-rep reg0))
  (asm "ret i32 0")
  (asm "}")
   )

(define (label l)
  ;;(note "label ~a" (symbol->asm-id l))
  (asm "~a:" (symbol->asm-id l))
  ;;(asm "; <label>:~a: " (symbol->asm-id l))
  )

(define (sar a b)
  (binary-op 'ashr a b ) )

(define (sal a b)
  (binary-op 'shl a b ))

(define (shl a b)
  (binary-op 'shl a b ))

(define (shr a b)
  (binary-op 'lshr a b ) )

(define (land a b)
  (binary-op 'and a b ))

(define (xor a b)
  (binary-op 'xor a b ) )


(define (save a)
  (note "push ~a" (operands-rep a)))

(define (restore a)
  (note "pop ~a" (operands-rep a)))

(define (nop)
  (asm "(nop)"))



)