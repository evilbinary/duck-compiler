;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (egg)
  (export 
    asm-gen-file
    asm-gen
    represent-conversion
    flatten-conversion
    instruct-conversion
    assign-conversion
    restruct-instruct-body
    remove-local
  )

(import (scheme)
    (match)
    (trace)
    (common)
    (arch)
    (primitive)
    (type)
    )


(define symbols (get-sym))

(define (clear-gen-symbol)
  (clear-sym)
)


(define (represent-conversion exp)
  (note "represent-conversion ~a" exp)
  (match exp
    [() `null-rep ]
    [(void) `null-rep ]
    [#t `true-rep ]
    [#f `false-rep ]
    [(let ((,v ,exp1)) ,exp2)
      (printf "==>let ~a ~a\n" v exp1 )
      `(let ([,v ,(represent-conversion exp1) ])
          ,(represent-conversion exp2) )
    ]
    [,v
      (guard (string? v))
      (note "alloc string ~a" v)
      `(alloc ,v)
    ]
    [,v (guard (integer? v))
      (note "integer  ~a" v )
      (type-rep v)
    ]
    [(if ,e1 ,e2 ,e3)
      `(if ,(represent-conversion e1) ,(represent-conversion e2) ,(represent-conversion e3))
    ]
    [(quote ,e)
      (note "quote ~a" e)
      (cond
        [(integer? e) (represent-conversion e)]
        [(boolean? e) (represent-conversion e)]
        [(null? e) (represent-conversion e)]
        [(equal? (void) e) (represent-conversion e)]
        ; [(pair? e)  
        ;   (note "pair ~a . ~a" (car e) (cdr e) )
        ;   `(cons ,(represent-conversion (car e)) ,(represent-conversion `(quote ,(cdr e))) )
        ; ]
        [else 
        `(alloc ,(represent-conversion e))
        ])
    ]
    [,v 
      (guard (symbol? v))
      (note "symbol ~a" v)
      v
    ]
    [(,binop ,arg1 ,arg2)
      (guard (binop? binop))
      `(,binop ,(represent-conversion arg1) ,(represent-conversion arg2) )
    ]
    [(,prim ,args ...)
      (guard (prim? prim))
      (note "prim ~a" prim)
      `(,prim ,@(map represent-conversion args ))
    ]
    [(,app ,args ...)
      `(,app ,@(map represent-conversion args ))
    ]
    [,exp 
      (printf "unkown exp->~a\n" exp)
    exp]
  )
)
;;anf to flatten program
;; code ,data
(define (flatten-conversion exp)
  (define (flatten exp)
    (match exp
      [(let ((,proc (lambda (,args ...) ,body ... ))) ,e)
        (flatten `(codes
            (proc ,proc (local ,args ,@body) )
            ,(flatten e)
          ))
      ]
      [(let ((,var ,e1)) ,e2)
        (printf "let ~a ~a\n" e1 e2)
        (flatten  `(codes 
            (local (,var)
              (set ,var ,e1)
              ,(flatten e2)
            )
          ))
      ]
      [,v 
        (guard (symbol? v))
        v
      ]
      [(proc ,l ,args ,body)
        `(proc ,l ,args body)
      ]
      [(local ,args ...)
        `(local ,args ...)
      ]
      [(,app ,args)
        `(,app ,args)
        ; `(call ,app ,args)
      ]
      ; [(call ,args ...)
      ;   (printf "call ~a next->~a\n" args next)
      ;   `(call ,args ...)
      ; ]
      [(set ,v ,e)
        (printf "set ~a=~a\n" v e)
        `(set ,v ,e)
      ]
      [,exp 
        (printf "unkown exp->~a\n" exp)
        exp
      ]))

  (note "flatten-conversion ~a" exp)
  (flatten exp)
)

(define (instruct-conversion exp)
    (match exp
      [(codes ,c* ...)
        `(instruct ,@(map instruct-conversion c*) )
      ]
      [(proc ,var (local ,args ,e* ... ,last))
       ` (instruct 
          (proc ,var
            (local ,args
              ,@(map instruct-conversion e*)
              (set reg0 ,(instruct-conversion last))
              (ret))
          ))
      ]
      [(local ,args ,e* ...)
        `(local ,args ,@(map instruct-conversion e*) )
      ]
      [(set ,var (,app ,arg))
        `(instruct 
           (call ,app (arg ,arg)) ;;reg0
           (set ,var reg0)
        )
      ]
      [(set ,var ,val)
        `(set ,var ,(instruct-conversion val))
      ]
      [(+ ,a ,b)
      `(add ,a ,b)
      ]
      [(- ,a ,b)
        `(sub ,a ,b)
      ]
      [(,app ,args ...)
      ;;(printf "call app ~a ~a\n" app args)
      `(instruct
          (call ,app (arg ,@args))
        )
      ]
     [,var 
      (guard (symbol? var))
      var
     ]
      [,var 
      (guard (number? var))
      var
     ]
     [,exp 
        ;;(printf "unkown exp->~a\n" exp)
        (error 'instruct-conversion "unkow exp" exp)
        exp
      ]
  )
)

(define (assign-conversion exp)
  (define (lookup key env)
    (printf "lookup key=~a env=~a\n" key env)
    (cond
      [(assq key env) => cdr]
      [else key]))

  (define (ext key val env)
    (cond
      [(eq? key val) env]
      [(not (pair? val))
        (cons `(,key . ,val) env)]
      [else env]))

  
  (define (assign cur env)
    (match cur
      [(instruct ,c* ...)
          `(instruct ,@(map (lambda (i) 
              (printf "env=> ~a\n" env)
              (assign i env) ) c*) )
      ]
      [(proc ,var (local ,args ,x ...))
       `(proc ,var
            ,(assign `(local ,args ,x ...) env)
          )
      ]
      [(local ,args ,e* ...)
        (let ((rargs (let loop ((offset 0) (i args) (of '()))
                (if (pair? i)
                  (begin 
                    (set! env (ext (car i) offset env) )
                    (printf "env ~a\n" env)
                    (loop (+ offset 1) (cdr i) (append of (list offset)) ))
                  of
                )
              )))
            (if (null? rargs)
              `(local ,args ,@e*)
              `(local 
                ,rargs
                ; ,args 
                ,@(map (lambda (i) (assign i env)) e*)
              )))
      ]
      [(add ,a ,b)
        `(add ,(if (number? a) a `(local ,(lookup a env)))
              ,(if (number? b) b `(local ,(lookup b env)))
          )
      ]
      [(set ,a ,b)
        `(set ,(assign a env) ,(assign b env))
      ]
       [(arg ,a ,e* ...)
       (note "arg ~a ~a" a e*)
        (let ((it (lookup a env)))
          (if (equal? it a)
            `(arg ,a ,@(map (lambda (e)  
                (let ((i (lookup e env)))
                    (if (equal? i e)
                      e
                      `(local ,i) ))
              ) e* ))
            `(arg (local ,it) ,@(map (lambda (e)  
                (let ((i (lookup e env)))
                    (if (equal? i e)
                      e
                      `(local ,i)))
              ) e* ))
          )
        )
      ]
      ; [(arg ,a)
      ;   (let ((it (lookup a env)))
      ;     (if (equal? it a)
      ;       `(arg ,a)
      ;       `(arg (local ,it) )
      ;     )
      ;   )
      ; ]
      [(call ,app ,arg)
        (note "call ~a ~a" app arg)
        (let ((it (lookup app env)))
          (if (equal? it app)
            `(call ,app ,(assign arg env) )
            `(call (local ,it) ,(assign arg env))
          )
        )
      ]
      [,var 
        (guard (symbol? var))
        (let ((it (lookup var env)))
          (if (equal? it var)
            var
            `(local ,(lookup var env))))
      ]
      [,exp 
          (printf "assign-conversion unkown exp->~a\n" exp)
          exp
      ]
    ) 
  )
  (assign exp '())
)

(define (lib-asm-conversion exp)
    (remove-local (assign-conversion exp))
)

;;add proc to last 
(define (restruct-instruct-body exp)
  (let ((defs '() ) 
        (text '())
        (datas `(
          (sdata)
          (data ,(symbol->asm-id 'true-rep) ,true-rep)
          (data ,(symbol->asm-id 'false-rep) ,false-rep)
          (data ,(symbol->asm-id 'void-rep) ,void-rep)
          (data ,(symbol->asm-id 'null-rep) ,null-rep)
          ))
        (libs (lib-asm-conversion `(instruct
          (instruct ,(print-value))
          (instruct ,(print-dot))
          (instruct ,(print-list))
           )))
       )
       
    (define (collect cur)
      (match cur
      [(instruct (proc ,var) ,e* ...)
        (printf "instruct ~a\n" cur)
        (if (null? defs)
          (set! defs `((proc ,var) ,e* ... ))
          (set! defs `((proc ,var) ,e* ... ,defs ))
        )
        ;;(printf "lllllabel ->~a\n" label)
        `(instruct )
      ]
      [(instruct (data ,var ,val) ,e* ...)
        (printf "instruct ~a\n" cur)
        (set! datas `((data ,var ,val) ,e* ... ,@datas ))
        `(instruct )
      ]
      [(instruct ,c* ...)
        `(instruct ,@(map collect c*))
      ]
      [,exp 
        exp
      ]
      )
    )
    (set! text (collect exp))
    `(instruct
      (stext)
      ,text
      (sexit 0)
      ,(if (null? defs)
        `(instruct ,libs)
        `(instruct ,@defs ,libs
        )
      )
      ,(if (null? datas)
        `(instruct)
        `(instruct ,@datas)
      )
    )
  )
)


(define (remove-local exp)
  (match exp
    [(instruct (proc ,var (local ,args ,e* ...)))
      `(instruct (proc ,var) ,@e*)
    ]
    [(instruct (local ,args ,e* ...))
    `(instruct ,@(map remove-local e*))
    ]
    [(instruct ,c* ...)
      `(instruct ,@(map remove-local c*) )
    ]
    ; [(,app ,a)
    ;   `(,app ,(remove-local a))
    ; ]
    [,exp
      (printf "unkown exp->~a\n" exp)
      exp
    ]
))

;; flatten code 
;; (code ....)
(define (emit-code exp env)
  (define (emit-p cur env)
    (note "inst ~a" cur)
    (match cur
      [(instruct ,ints ...)
        ;;(note "ints=>~a " ints)
        (let loop [(i ints)]
            (if (pair? i)
              (begin 
                ; (note "====> ~a" (car i))
                (emit-code (car i) env)
                (loop (cdr i))
              )
            )
        )
       ]
      [,atom
        (guard (number? atom))
        atom
      ]
      [,atom
        (guard (string? atom))
        atom
      ]
      [,type
        (guard (type? type))
        type
      ]
      [,reg
        (guard (memq reg regs-map))
        ;;(printf "reg=>~a\n" reg)
        reg
      ]
      [(label ,var)
        (label var)
      ]
      [(proc ,var)
        (proc var)
      ]
      [(local ,index)
        (local index)
      ]
      [(set ,var ,val)
        (let ((l  (emit-p var env) )
            (r (emit-p val env)))
          (if (void? r)
            (set! r reg0)
          )
          (if (void? l)
            (set! l reg0)
          )
          (set l r)
          )
      ]
      ;;prim call 
      [(call ,app (arg ,args ...) )
        (guard (prim? app))
        (note "prim call ~a" app)
        (emit-prim app (map  (lambda (e)
            (emit-p e env)) args ))        
      ]

      [(call ,app (arg ,args ...))
        ; (arg (emit-p args env))
        ;(printf "call app= ~a ~a\n" app args)
        (apply call app 
          (map  (lambda (e)
            (emit-p e env)) args ))
      ]
      [(call ,app)
        (call app )
      ]
      [(ret)
        (ret)
      ]
      [(add ,a ,b)
        ;;(add (emit-p a env) (emit-p b env))
        (set reg0 (emit-p a env))
        (sar reg0 type-shift)
        (set reg1 (emit-p b env))
        (sar reg1 type-shift)
        (add reg0 reg1)
        (sal reg0 type-shift)
      ]
      [(sar ,a ,b)
        (sar a b)
      ]
      [(note ,x ...)
        (note x)
      ]
      [(land ,a ,b)
        (land a b)
      ]
      [(cmp-jmp ,val1 ,val2 ,l1 ,l2)
        (cmp-jmp val1 val2 l1 l2)
      ]
      [(jmp ,l)
        (jmp l)
      ]
      [(save ,a)
        (save a)
      ]
      [(restore ,a)
        (restore a)
      ]
      [(mref ,a ,b ,x ...)
        (mref a b x)
      ]
      ;;primitive start
      [(emit-print-value (list ,a))
        (emit-print-value (list a))
      ]
      [(emit-printf ,fmt  ,args ...)
        (apply emit-printf fmt args)
      ]
      [(emit-print-type-value ,tag  ,str ,lend)
        (emit-print-type-value tag  str lend)
      ]
      [(emit-print-const-value ,tag  ,str ,lend)
        (emit-print-const-value tag  str lend)
      ]
      [(print-value) (print-value)]
      [(print-list) (print-list)]
      [(print-dot) (print-dot)]


      [(sdata)
        (sdata)
      ]
      [(data ,a ,b)
        (data a b)
      ]
      [(stext)
        (stext)
      ]
      [(sexit ,code)
        (sexit code)
      ]
      [,?
        (error 'emit-code "gen asm unknow " ?)
      ]
  ))
  (emit-p exp env)
)

(define (asm-gen exp)
  ;;(stack-trace-exception)
  (clear-gen-symbol)

  (let ((out-asm 
      (with-output-to-string
        ; (
          (lambda () 
           (try
              (let ((env '())) 
                (note "gen by Lisp依依鸭 evilbinary on ~a" (date-and-time))
                (note exp)
                (note "code start")
                (emit-code exp env)
                (note "code end")                
              )
              (catch (lambda (x) 
                (display-condition x) 
              )))

            ))))
    ;;(printf "out-asm \n~a\n" out-asm)
    out-asm)  
  )

  (define (asm-gen-file exp name)
    (let ([o (open-file-output-port (format "~a.s" name) (file-options replace)
                (buffer-mode block) (current-transcoder))])
          (display (asm-gen exp) o)  
      (close-output-port o))
    (asm-compile-exp exp name)
  )  

)