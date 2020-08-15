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
    restruct-block
    instruct-optimize
    remove-code
  )

(import
    (rename (scheme) (div div2) )
    (match)
    (trace)
    (common)
    (arch)
    (libs)
    (type)
    (options)
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
    [($asm ,args ...)
      `($asm ,@args)
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

;;anf let to flatten program
(define (flatten-conversion exp)
  (define (find-vars cur vars)
    (match cur
      [(let ((,var ,e1)) ,e2)
        (printf "find-vars let=>~a ~a ~a\n" var e1 e2)
        (set! vars (append vars (list var) (find-vars e2 vars)))
        (printf "vars=>~a\n" vars)
        vars
      ]
      [(if ,a ,b ,c)
        (set! vars (append vars
          (find-vars a vars)
          (find-vars b vars)
          (find-vars c vars)))
        vars
      ]
      [(,app ,args ...)
        (printf "find app ==> ~a ~a\n" app args)
        vars
      ]
      [,v
        (guard (or (number? v) (symbol? v)))
        (printf "symbol/number ==> ~a\n" v)
        vars
      ]
      [,exp 
        ;;(printf "unkown exp->~a\n" exp)
        (error 'flatten "find-vars erro" exp )
        vars
      ]
    )
  )

  (define (flatten exp)
    (printf "flatten===>~a\n" exp)
    (match exp
      [(let ((,proc (lambda (,args ...) ,body ))) ,e)
        (printf "proc==> ~a args=~a body=~a  e=~a\n" proc args  body  e)
        `( (program ,proc
              ,args ;;args
              ,(find-vars body '()) ;;vars
              ,@(flatten body))
            (program main
              () ;;args
              (,proc ,@(find-vars e '())) ;;vars
              ,@(flatten e))
          ) 
      ]
      [(let ((,var ,e1)) ,e2)
        (printf "let ==> ~a ~a ~a\n" var e1 e2)
        `((let ((,var ,e1)) ,e2))
      ]
      [(if ,a ,b ,c)
       `((if ,a ,b ,c))
       ]
      [(,app ,args ...)
        (printf "app ==> ~a ~a\n" app args)
        `((,app ,@args))
      ]
      [,v
        (guard (or (number? v) (symbol? v)))
        (printf "symbol/number ==> ~a\n" v)
        `(,v)
      ]
      [,exp 
        ;;(printf "unkown exp->~a\n" exp)
        (error 'flatten "flatten erro" exp)
      ]
    ))

  ;;program main start
  `(program all
      () ;;args
      ,(find-vars exp '()) ;;vars
      ,@(flatten exp))
)


(define (remove-code e)
    (define result '())
    (define (collect-exp ex)
      (set! result (append result (list ex)))
    )
    (define (match-exp ex)
      (match ex
        [(,app ,args ...)
          (guard (not (equal? app 'code)))
          #t
        ]
        [,v #f]
      )
    )

    (define (remove-p e k)
      (printf ">>>>>>>>>>remove-p ~a\n" e)
      (match e
        ; [(code (,apps ...))
        ;   (printf " remove-code1 ~a\n" apps)
        ;   (k `( ,@apps))
        ; ]
        [(code ,exps ...)
          (printf " remove-code3 ~a\n" e)
          (let ((ret (remove-p* exps id) ))
            ; (if (match-exp ret)
            ;   (k `(code ,@ret ))
            ;   (k `( ,@ret))
            ; )
            (k `(code ,@ret))
          )
        ]
        [(,apps ...)
          (k `(,@apps))
        ]
        [,v
          (guard (or (number? v) (symbol? v)))
          (k v)
        ]
        [,exp 
        (printf "unkown exp->~a\n" exp)
        (error 'remove-code "remove-code erro" exp)
      ]
      )
    )

  (define (remove-p* exp* k)
    (printf "=======>remove-p* ~a\n" exp* )
    (if (null? exp*)
      (begin 
        (printf "----------->exps end exp*=~a\n" exp*)
        (k exp*)
      )
      (remove-p (car exp*) 
        (lambda (t)
          (printf " (car exp*)=~a ret=>~a match=~a\n" (car exp*) t (match-exp t))  
          (if (match-exp t)
            (collect-exp t)
          )        
          (remove-p* (cdr exp*)
            (lambda (t*) 
              (printf "=======>t=~a t*=~a\n" t t*)
              (k `(,t . ,t*)))) ))
    )
  )
  (remove-p e id)
  result
)

;;conver codes =>instruct
(define (instruct-conversion exp)
  (printf "instruct-conversion ~a\n" exp)
  (match exp
    [(program ,name ,vars ,args ,e ...)
      (printf "   program=> ~a ~a ~a ~a\n" name vars args e)
      (let ((instructs (map instruct-conversion e)))
        (printf "   instructs=>~a\n" instructs)
        (set! instructs (remove-code `(code ,@instructs)) )
        (printf "   instructs2=>~a\n" instructs)
        `(program ,name ,vars ,args 
            ,@instructs ))
    ]
    [(let ((,var ,e1)) ,e2)
      (printf "let ==> ~a ~a ~a\n" var e1 e2)
      `(code
        ,(instruct-conversion e1)
        (set ,var reg0)
        ,(instruct-conversion e2) 
        )
    ]
    [(if ,a ,b ,c)
        (let ((l1 (gen-sym 'ifa))
              (l2 (gen-sym 'ifb))
              (l3 (gen-sym 'ifend)))
        `(code
            ,(instruct-conversion a)
            (cmp-jmp reg0 false-rep ,l2 ,l1)
            (label ,l1)
            ,(instruct-conversion b)
             (jmp ,l3)
            (label ,l2)
            ,(instruct-conversion c)
            (label ,l3)
          )
        )
    ]
    [(set ,var (,e ...) )
      (printf "set1 ~a ~a\n" var e)
      `(code
        ,(instruct-conversion `(,@e) )
        (set ,var reg0)
      )
    ]
    [(set ,var ,val)
      (printf "set2 ~a ~a\n" var val)
      `(code
        (set ,var ,val)
      )
    ]
    [($asm ,args ...)
      `($asm ,@args)
      ]
    [(,app ,args ...)
      (guard (prim? app))
      `(code (,app ,@args ))
    ]
    [(tail ,app ,args ...)
      `(code (,app ,@args))
    ]
    [(,app ,args ...)
      `(code (,app ,@args))
    ]
    [,v
      (guard (or (number? v) (symbol? v)))
      (printf "symbol ==> ~a\n" v)
       `(code (set reg0 ,v))
      ]
    [,exp 
      ;;(printf "unkown exp->~a\n" exp)
      (error 'instruct-conversion "instruct-conversion erro" exp)
    ]
  )    
)

(define (instruct-optimize exp)
  (match exp
      [(program ,name ,vars ,args ,e ...)
      (printf "   program=> ~a ~a ~a ~a\n" name vars args e)
      (let ((instructs (map instruct-optimize e)))
        (printf "instructs=>~a\n" instructs)
        `(program ,name ,vars ,args 
            ,@instructs ))
    ]
    [(set ,var (set ,var2 ,val))
      (if (equal? var var2 )
        `(set ,var ,val)
        `(set ,var (set ,var2 ,val))
      )
    ]
    [,v
      v
    ]
  )
)

(define (assign-conversion exp)
  (define (lookup key env)
    (printf "   lookup key=~a env=~a\n" key env)
    (cond
      [(assq key env) => cdr]
      [else '()]))

  (define (lookup-value val env)
    (printf "   lookup val=~a env=~a\n" val env)
    (cond
      [(> (length (filter (lambda(x) (= 0 (cdr x))) env)) 0) val]
      [else '()]))

  (define (ext key val env)
    (cond
      [(eq? key val) env]
      [(not (pair? val))
        (cons `(,key . ,val) env)]
      [else env]))

  (define genv '())
  
  (define (assign cur env)
    (match cur
      [(program ,name ,args ,vars ,c* ...)
          (set! genv (ext  name (length genv) genv))
          (let ((lenv '()))
            (mapc (lambda (x)
                (set! lenv (ext  x (length lenv) lenv))
                ) args)
            (mapc (lambda (x)
                (set! lenv (ext  x (length lenv) lenv))
                ) vars)
            (printf "prgram name=~a args vars env =>~a\n" name lenv)
          `(program ,name ,@(map (lambda (i) 
              (printf "   env=> ~a\n" lenv)
              (assign i lenv) ) c*) )
          )
      ]
      [($asm ,args ...)
      `($asm ,@args)
      ]
      [(,binop ,a ,b)
        (guard (memq binop '(> < >= < <= = + - * /) ))
        `(,binop ,(assign a env)
                  ,(assign b env)
          )
      ]
      [(set ,a ,b)
        `(set ,(assign a env) ,(assign b env))
      ]
      [(,app ,args ...)
        `(,(assign app env)
          ,@(map (lambda (x) 
                (assign x env)
             ) args ))
      ]
      [,var 
        (guard (symbol? var))
        (printf "var==>~a env=~a genv=~a\n" var env genv)
        (let ((it (lookup var env))
              (git (lookup var genv)) )
          (if (null? git)
            (if (null? it)
              var
              `(local ,it )
            )
            var
             ) )
      ]
      [,exp 
          (printf "assign-conversion unkown exp->~a\n" exp)
          exp
      ]
    ) 
  )
  (assign exp genv)
)


;;add proc to last 
(define (restruct-block exp)
  
  (let ((block-defs '() ) 
        (block-main '())
        (block-data `(
          (data ,(symbol->asm-id 'true-rep) ,true-rep)
          (data ,(symbol->asm-id 'false-rep) ,false-rep)
          (data ,(symbol->asm-id 'void-rep) ,void-rep)
          (data ,(symbol->asm-id 'null-rep) ,null-rep)
          ))
        (block-libs `(
          ,(assign-conversion `,(print-value))
          ; (assign-conversion ,(print-dot))
          ,(assign-conversion `,(print-list))
           ))
       )
  (define (collect cur)
    (match cur
      [(program all ,bodys ...)
        `(program all ,@(mapc collect bodys) )
      ]
      [(program ,name ,bodys ... )
        (printf "programe-> name=~a body=~a\n" name bodys)
        (if (equal? 'main name)
          (set! block-main (append block-main  bodys ))
          (set! block-defs (append block-defs `(block ,name ,@bodys )))
        )
        `(program ,name ,@bodys )
      ]
      ;;default in main
      [,exp  
        (set! block-main (append  block-main `(,exp)  ))
        exp
      ]
    )
  )
  (collect exp)

  `(block all 
    ;;main block
    (block main
      (stext)
      ,@block-main
      (sexit 0)
    )
    ;;defs block
    ,block-defs
    ;;
    ,@(if need-primitive block-libs `() )
    ;;data block
    (block data
      (sdata)
      ,@block-data
    )
  )
)   
)

;; flatten code 
;; (code ....)
(define (emit-code exp env)
  (define (emit-p cur env)
    (note "emit-code ~a" cur)
    (match cur
      [(block ,name ,blocks ...)
        (let ((is-main (memq  name '(main all data ))))
        (if (not is-main)
          (proc name))
        (let loop [(i blocks)]
            (if (pair? i)
              (begin 
                ; (note "====> ~a" (car i))
                (emit-code (car i) env)
                (loop (cdr i))
              )
            )
        )
        (if (not is-main)
          (ret))
          )
       ]
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

      [,atom 
        (guard (null? atom))
        (note "null")
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
      [(ret)
        (ret)
      ]
      [(cmp-jmp ,a ,b ,c)
        (cmp-jmp a b c)
      ]
      [(,binop ,a ,b)
        (guard (memq binop '(> < >= < <= =) ))
        (cmp binop (emit-p a env) (emit-p b env))
      ]
      ; [(,binop ,a ,b)
      ;   (guard (memq binop '(sub mul div) ))
      ;   (binop (emit-p a env) (emit-p b env))
      ; ]
      [(,binop ,a ,b)
        (guard (memq binop '(+ add)))
        ;(add (emit-p a env) (emit-p b env))
        (set reg0 (emit-p a env))
        ; (sar reg0 type-shift)
        (set reg1 (emit-p b env))
        ; (sar reg1 type-shift)
        (add reg0 reg1)
        ; (sal reg0 type-shift)
      ]
      [(,binop ,a ,b)
        (guard (memq binop '(* mul)))
        (mul (emit-p a env) (emit-p b env))
      ]
      [(,binop ,a ,b)
        (guard (memq binop '(- sub)))
        (sub (emit-p a env) (emit-p b env))
      ]
      [(,binop ,a ,b)
        (guard (memq binop '(/ div)))
        (div (emit-p a env) (emit-p b env))
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
      [(emit-print-value ,args ...)
        (apply emit-print-value args)
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

      [($asm ` ,e  )
        (note "$asm ~a" e)
        (emit-p e env)
      ]
      [($asm ,args ...)
        (note "$asm ~a" (map  (lambda (e)
            (emit-p e env)) args ) )
        (apply asm (map  (lambda (e)
            (emit-p e env)) args ))   
      ]
      ;;prim call 
      [(,app ,args ...)
        (guard (prim? app))
        (note "prim call ~a" app)
        (emit-prim app (map  (lambda (e)
            (emit-p e env)) args ))        
      ]

      ;;direct call
      [(call ,app ,args ...)
        (guard (pair? app))
        (set reg1 (emit-p app env) )
        (apply call reg1 
          (map  (lambda (e)
            (emit-p e env)) args ))
      ]
      [(call ,app ,args ...)
        ; (arg (emit-p args env))
        ;(printf "call app= ~a ~a\n" app args)
        ;;call name
        (apply call app 
          (map  (lambda (e)
            (emit-p e env)) args ))
      ]
      [(call ,app)
        (call app )
      ]

      ;;direct call
      [(,app ,args ...)
        (guard (pair? app))
        (emit-p `(call ,app ,@args) env)
      ]
      [(,app ,args ...)
        (emit-p `(call ,app ,@args) env)
      ]
      [(,app)
        (emit-p  `(call ,app) env)
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