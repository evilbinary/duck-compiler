;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (primitive)
  (export 
    print-dot
    print-list
    print-value

    emit-print-const-value
    emit-print-type-value
    emit-print-value
    emit-printf
    emit-alloc
    emit-free
    emit-prim
    prim?
    binop?
  )

(import 
    ;;(scheme)
    (rename (scheme) (div div2) )
    (match)
    (trace)
    (common)
    (arch)
    (type)
    )


(define (prim? prim)
  (memq prim '(printc void cons car cdr pair? null? alloc free) )
)

(define (binop? op)
  (memq op '(+ - * / ) )
)

;;asm inst
(define (emit-print-type-value tag  str lend)
  (let ((l1 (gen-sym 'ltype))
        (l2 (gen-sym 'ltype)))
        (set reg0 reg1)
        (land reg0 type-mask)
        (cmp-jmp reg0 tag l1 l2)
        (label l1)
        (set reg0 reg1)
        (sar reg0 type-shift)
        (emit-printf str reg0)
        (jmp lend)
        (label l2)
      )
)

(define (emit-print-const-value tag  str lend)
  (let ((l1 (gen-sym 'lconst))
        (l2 (gen-sym 'lconst))
        (l3 (gen-sym 'lconst)) )
        (set reg0 reg1)
        (land reg0 type-mask)
        (cmp-jmp reg0 const-tag l1 l2)
        (label l1) ;;is const
        (set reg0 reg1)
        (land reg0 const-mask)
        (sar reg0 type-shift)
        (cmp-jmp reg0 tag l3 l2)
        (label l3) ;; is tag
        (emit-printf str reg0)
        (jmp l2)
        (label l2)
      )
)

(define (emit-printc fmt . args)
  (note "printc ~a args=~a"  fmt args)
  (apply ccall 'printf fmt args)
)

(define (emit-printf fmt . args)
  (note "printf fmt=~a args=~a fmt string?=~a"  fmt args (string? fmt))
  ; (data (symbol->asm-id fmt) fmt)
  (let ((str (if (string? fmt) (string-append "fmt" fmt) fmt) )) 
  (note "here")
    (asm-data-add str fmt)
    (save reg0)
    (apply fcall 'printf str args)
    (restore reg0)
    (note "printf end")
  )
)

(define (word-size n)
  (flonum->fixnum (* n (/ arch-bits 8.0)))
)

(define (emit-cons args)
  (note "emit-cons ~a " args)
  (emit-alloc (word-size 2)) ;;car cdr
  (set reg4 reg0) ;;save reg0
  
  ; (emit-printf "emit-alloc=%x " reg0)
  ; (set reg0 reg4)

  (set reg5 (car args) "reg5=a" )
  (mset reg0 reg5 "[reg0]=reg5=a set-car") 
  (add reg0 (word-size 1))
  (set reg5 (cadr args) "reg5=b")
  (mset reg0 reg5 "[reg0+1]=reg5=b set-cdr") ;;set cdr
  (set reg0 reg4 "reg0=malloc addr")
  (sal reg0 type-shift)
  (note "add reg0 pair-tag")
  (add reg0 pair-tag)
  
  (note "end emit-cons")
  )

(define (emit-print-value . args)
  (note "emit-print-value ~a " args)
  (apply call "print-value" args)
  )


(define (emit-alloc val)
  (note "emit-alloc ~a" val)
  (cond
    [(string? val) 
      (note "emit-alloc string ~a" val)
      ; (foreign-call 'malloc (string-length val))
      ; (set reg2 (string-length val) ";;count")
      ; (set reg4  ";;si")
      ; (set reg5  ";;di")
      ; (call 'copy-string)
      (let ((str (string-append "str." (symbol->asm-id val) ) ))
        (asm-data-add str val)
        (set reg0 str)
        (sal reg0 type-shift)
        (add reg0 string-tag)
      )
    ]
    [(symbol? val)
      (note "emit-alloc symbol ~a" val)
      (let ((str (string-append "sym." (symbol->asm-id (symbol->string val) ) )))
        (asm-data-add str val)
        (set reg0 str)
        (sal reg0 type-shift)
        (add reg0 symbol-tag)
      )
    ]
    [(number? val)
      (note "emit-alloc size ~a" val)
      ;(set reg0 0)
      (fcall 'malloc val)
      (set reg1 reg0)
      
      ; (emit-printf "malloc=%x" reg0)
      ; (set reg0 reg1)

      (note "end emit-alloc size ~a" val)
    ]
    ; [(pair? val)
    ;   val
    ; ]
    [else (error 'emit-alloc "not support" val) ]
  )
)


(define (emit-free a)
  (note "free ~a" a)
  (fcall 'free a)
)


;;prim
(define (emit-prim app args)
    (case app
        ['printc
          (apply emit-printc  args)
          ]
        ['void "" ]
        ['cons (emit-cons args)]
        ['car 
          (note "car ~a" args)
          (set reg2 (car args))
          (sar reg2 type-shift)
          (mref reg0 reg2 "car pair")
          ; (emit-print-value (list reg0))
        ]
        ['cdr 
          (note "cdr ~a" args)
          (set reg2 (car args))
          (sar reg2 type-shift)
          (note "add reg2 word-size 1")
          (add reg2 (word-size 1))
          (mref reg0 reg2 "cdr pair")
        ]
        ['pair?
          (let ((l1 (gen-sym 'pair))
                (l2 (gen-sym 'pair))
                (l3 (gen-sym 'pair))
                )
              (set reg2 (car args))
              (land reg2 type-mask)
              (cmp-jmp reg2 pair-tag l1 l2)
              (label l1)
              (set reg0 true-rep)
              (jmp l3)
              (label l2)
              (set reg0 false-rep)
              (label l3)
           )
        ]
        ['null?
          (let ((l1 (gen-sym 'null))
                (l2 (gen-sym 'null))
                (l3 (gen-sym 'null))
                )
              (set reg2 (car args))
              (cmp-jmp reg2 null-rep l1 l2)
              (label l1)
              (set reg0 true-rep)
              (jmp l3)
              (label l2)
              (set reg0 false-rep)
              (label l3)
           )
        ]
        ['alloc 
          (apply emit-alloc args)
        ]
        ['free
          (apply emit-free args)
        ]
    )
    )

(define-syntax define-primitive
  (syntax-rules ()
   [(_ (prim-name arg* ...) body body* ...)
    (define prim-name
        (lambda () 
            `(block (label prim-name arg* ... )  body body* ... ) ;;(local arg* ... )
        )
      )
  ]))

  (define-primitive (print-dot arg0)
    (set reg0 (local 0))
    ;;(set reg0 reg1)
    (sar reg0 ,type-shift)
    (set reg2 reg0)
    (save reg0)
    (save reg1)
    (save reg2)

    (mref reg0 reg2 "car pair")
    (emit-print-value  reg0)

    (restore reg2)
    (restore reg1)
    (restore reg0)

    (set reg0 reg1)
    (sar reg0 ,type-shift)
    (set reg2 reg0)
    (add reg2 ,(word-size 1) )
    (mref reg0 reg2 "cdr pair")
    
    (note "is null?")
    (cmp-jmp reg0 null-rep print-dot-l4 print-dot-l3)
    (label print-dot-l3)

    (save reg0)
    (save reg1)
    (save reg2)
    (emit-print-value reg0)
    (restore reg2)
    (restore reg1)
    (restore reg0)
    (label print-dot-l4)
    (lret)
)

(define-primitive (print-list arg0)
    (save reg0)
    (save reg1)
    (save reg2)
    ;;save arg0 to reg1
    (set reg1 (local 0))
    (set reg0 reg1)

    (jmp print-list-l1)
    (label print-list-l1)
    
    (note "is pair?")
    (land reg0 ,type-mask)
    (cmp-jmp reg0 ,pair-tag print-list-l2 print-list-lend)

    ;;reg0 is pair
    (label print-list-l2)
    ;;reg0=car(pair)
    (set reg0 reg1)
    (sar reg0 ,type-shift)
    (set reg2 reg0)
    (mref reg0 reg2 "reg0=car pair")
    ;;print car value
    (emit-print-value  reg0)

    ;;reg1=cdr(pair)
    (set reg0 reg1)
    (sar reg0 ,type-shift)
    (add reg0 ,(word-size 1))
    (mref reg1 reg0 "reg1=cdr pair")
    
    ;;reg1=cdr(pair)
    ;;is cdr ==0
    (set reg0 reg1)
    (note "is end break")
    (cmp-jmp reg0 0 print-list-lend print-list-l3)
    (label print-list-l3)
    (note "check reg1=cdr(pair) is null")
    (cmp-jmp reg0 null-rep print-list-lend ())
    (note "check reg1=cdr(pair) is pair")
    (land reg0 ,type-mask)
    (cmp-jmp reg0 ,pair-tag print-list-l4 print-list-l6)
    ;;reg0 =cdr(pair) is pair
    (label print-list-l4)
    (emit-printf " ")
    (jmp print-list-l8)
    ;; is dot
    (label print-list-l6)
    (emit-printf " . ")
    (emit-print-value  reg1)
    
    (jmp print-list-l8)
    (label print-list-l8)
    ;;loop next
    (set reg0 reg1)
    (jmp print-list-l1)
    
    (label print-list-lend)
    (restore reg2)
    (restore reg1)
    (restore reg0)

    (lret)
)

(define-primitive (print-value arg0)
      ;;param reg0
      ;;save param0
      (save reg0)
      (save reg1)
      (save reg2)
      (set reg1 (local 0))
      ;;print pair
      (note "print pair")
      ; (let ((l1 (gen-sym 'lpair))
      ;       (l2 (gen-sym 'lpair))
      ;       (l3 (gen-sym 'lpair))
      ;       (l4 (gen-sym 'lpair)))
        (set reg0 reg1)

        (land reg0 ,type-mask)
        (cmp-jmp reg0 ,pair-tag print-value-l1 print-value-l2)
        (label print-value-l1)
        (note "print pair")
        (set reg0 reg1)
        (sar reg0 ,type-shift)
        
        (emit-printf "(")

        ;;(call "print-dot"  (arg reg0))   
        ;;reg1 is origin value
        (call "print-list" reg1)
        
        (emit-printf ")")
        (jmp print-value-lend)
        (label print-value-l2)
      ; )


      ;; print int
      (note "print int")
      (emit-print-type-value ,fixnum-tag  "%d" print-value-lend)

      ;;print symbol
      (note "print symbol")
      (emit-print-type-value ,symbol-tag "%s" print-value-lend)

      ;;print string
      (note "print string")
      (emit-print-type-value ,string-tag "%s" print-value-lend)

      ;;print boolean
      (note "print boolean")
      (emit-print-const-value ,true-tag "#t" print-value-lend)
      (emit-print-const-value ,false-tag "#f" print-value-lend)

      ;;print void
      (note "print void")
      (emit-print-const-value ,void-tag "(void)" print-value-lend)
      ;;print null
      (note "print null")
      (emit-print-const-value ,null-tag "()" print-value-lend)

      (jmp print-value-lend)
      (label print-value-lend)

      (restore reg2)
      (restore reg1)
      (restore reg0)
      (lret)
)


)