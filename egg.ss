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
    (logger)
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
      (log-debug "==>let ~a ~a" v exp1 )
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
    [(lambda () ,e)
    `(lambda () ,(represent-conversion e))
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
      (log-debug "unkown exp->~a" exp)
    exp]
  )
)

;;anf let to program
(define (flatten-conversion exp)
  (define (find-vars cur vars)
    (match cur
      [(let ((,var ,e1)) ,e2)
        (log-debug "find-vars let=>~a ~a ~a" var e1 e2)
        (set! vars (append vars (list var) (find-vars e2 vars)))
        (log-debug "vars=>~a" vars)
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
        (log-debug "find app ==> ~a ~a" app args)
        vars
      ]
      [,v
        (guard (or (number? v) (symbol? v)))
        (log-debug "symbol/number ==> ~a" v)
        vars
      ]
      [,exp 
        ;;(log-debug "unkown exp->~a" exp)
        (error 'flatten "find-vars erro" exp )
        vars
      ]
    )
  )

  (define (flatten exp)
    (log-debug "flatten===>~a" exp)
    (match exp
      [(let ((,proc (lambda (,args ...) ,body ))) ,e)
        (log-debug "proc==> ~a args=~a body=~a  e=~a" proc args  body  e)
        `(
           (program ,proc
              ,args ;;args
              ,(find-vars body '()) ;;vars
              ,@(flatten body))

              ; ,@(flatten e)
            (program main
              () ;;args
              (,proc ,@(find-vars e '())) ;;vars
              ,@(flatten e))
          )
      ]
      [(let ((,var ,e1)) ,e2)
        (log-debug "let ==> ~a ~a ~a" var e1 e2)
        `( (set ,var ,e1) ,@(flatten e2))
      ]
      [(if ,a ,b ,c)
       `((if ,a ,b ,c))
       ]
      [(,app ,args ...)
        (log-debug "app ==> ~a ~a" app args)
        `((,app ,@args))
      ]
      [,v
        (guard (or (number? v) (symbol? v)))
        (log-debug "symbol/number ==> ~a" v)
        `(,v)
      ]
      [,exp 
        ;;(log-debug "unkown exp->~a" exp)
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
      (log-debug ">>>>>>>>>>remove-p ~a" e)
      (match e
        ; [(code (,apps ...))
        ;   (log-debug " remove-code1 ~a" apps)
        ;   (k `( ,@apps))
        ; ]
        [(code ,exps ...)
          (log-debug " remove-code3 ~a" e)
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
        (log-debug "unkown exp->~a" exp)
        (error 'remove-code "remove-code erro" exp)
      ]
      )
    )

  (define (remove-p* exp* k)
    (log-debug "=======>remove-p* ~a" exp* )
    (if (null? exp*)
      (begin 
        (log-debug "----------->exps end exp*=~a" exp*)
        (k exp*)
      )
      (remove-p (car exp*) 
        (lambda (t)
          (log-debug " (car exp*)=~a ret=>~a match=~a" (car exp*) t (match-exp t))  
          (if (match-exp t)
            (collect-exp t)
          )        
          (remove-p* (cdr exp*)
            (lambda (t*) 
              (log-debug "=======>t=~a t*=~a" t t*)
              (k `(,t . ,t*)))) ))
    )
  )
  (remove-p e id)
  result
)

;;conver codes =>instruct
(define (instruct-conversion exp)
  (log-debug "instruct-conversion ~a" exp)
  (match exp
    [(program ,name ,vars ,args ,e ...)
      (log-debug "   program=> ~a ~a ~a ~a" name vars args e)
      (let ((instructs (map instruct-conversion e)))
        (log-debug "   instructs=>~a" instructs)
        (set! instructs (remove-code `(code ,@instructs)) )
        (log-debug "   instructs2=>~a" instructs)
        `(program ,name ,vars ,args 
            ,@instructs ))
    ]
    [(let ((,var ,e1)) ,e2)
      (log-debug "let ==> ~a ~a ~a" var e1 e2)
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
        ;;(log-debug "if ==> ~a ~a ~a" l1 l2 l3)
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
      (log-debug "set1 ~a ~a" var e)
      `(code
        ,(instruct-conversion `(,@e) )
        (set ,var reg0)
      )
    ]
    [(set ,var ,val)
      (log-debug "set2 ~a ~a" var val)
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
      (log-debug "symbol ==> ~a" v)
       `(code (set reg0 ,v))
      ]
    [,exp 
      ;;(log-debug "unkown exp->~a" exp)
      (error 'instruct-conversion "instruct-conversion erro" exp)
    ]
  )    
)

(define (instruct-optimize exp)
  (match exp
      [(program ,name ,vars ,args ,e ...)
      (log-debug "   program=> ~a ~a ~a ~a" name vars args e)
      (let ((instructs (map instruct-optimize e)))
        (log-debug "instructs=>~a" instructs)
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
    (log-debug "   lookup key=~a env=~a" key env)
    (cond
      [(assq key env) => cdr]
      [else '()]))

  (define (lookup-value val env)
    (log-debug "   lookup val=~a env=~a" val env)
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
          (set! genv (ext name (length genv) genv))
          (let ((lenv '()))
            (mapc (lambda (x)
                    (set! lenv (ext  x (length lenv) lenv))) 
                  args)
            (mapc (lambda (x)
                    (set! lenv (ext  x (length lenv) lenv)))
                  vars)
            (log-debug "prgram name=~a args vars env =>~a" name lenv)
          `(program ,name ,args ,@(map (lambda (i) 
              (log-debug "   env=> ~a" lenv)
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
        (log-debug "assign=~a ~a" app args)
        `(,(assign app env)
              ,@(map (lambda (x) 
                        (assign x env))
                     args ))
      ]
      [,var 
        (guard (symbol? var))
        (log-debug "var==>~a env=~a genv=~a" var env genv)
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
          (log-debug "assign-conversion unkown exp->~a" exp)
          exp
      ]
    ) 
  )
  (assign exp genv)
)

;;add proc to last 
(define (restruct-block exp)
  (log-set-level '())
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
      [(program all ,args ,bodys ...)
        (log-debug "programe all collect=> ~a" bodys )
        (set! block-main (append  (map collect bodys)  block-main  ))
      ]
      [(program ,name ,args ,bodys ... )
        (let ((b (map collect bodys) ))
          (log-debug "program-> name=~a body=~a" name bodys)
          (log-debug "  b==>~a block-main=>~a" b block-main)
          (if (equal? 'main name)
            (begin
              (set! block-main (append  b  block-main))
               '()
            )
            (begin 
              (set! block-defs (append block-defs (list `(block (function ,name ,@args) ,@bodys ))))
              ; `(block (function ,name ,@args) ,@bodys )
              `()
            )
          )
        )
        
      ]
      [($asm ,bodys ...)
        (log-debug "$asm ==>~a" bodys)
        `($asm ,@bodys)
      ]
      [(block ,args ...)
        `(block ,@args)
      ]
      ;;default in main
      [,exp  
        (log-debug "else==>~a block-main=>~a" exp block-main)
        exp
      ]
    )
  )
  (log-set-level 'info)
  (collect exp)

  `(block all 
    ;;main block
    (block main
      (stext start)
      ,@block-main
      (stext end)
    )
    ;;defs block
    ,@block-defs
    ;;
    ,@(if (option-get 'need-primitive) block-libs `() )
    ;;data block
    (block data
      (sdata start)
      ,@block-data
      (sdata end)
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
    [(block (label ,name ,args ...) ,blocks ...)
        (lproc name '())
        (let loop [(i blocks)]
            (if (pair? i)
              (begin 
                ; (note "====> ~a" (car i))
                (emit-code (car i) env)
                (loop (cdr i))
              )
            )
        )
       ]
      [(block (function ,name ,args ...) ,blocks ...)
        (note "block function ~a" name )
        (proc name '())
        (let loop [(i blocks)]
          (if (pair? i)
            (begin 
              ; (note "====> ~a" (car i))
              (emit-code (car i) env)
              (loop (cdr i))
            )
          )
        )
        (pret)
       ]
      [(block ,name ,blocks ...)
        (note "block name=~a" name )
        (let loop [(i blocks)]
              (if (pair? i)
                (begin 
                  ; (note "====> ~a" (car i))
                  (emit-code (car i) env)
                  (loop (cdr i))
                )
              )
          )
      ]
      [(sdata ,arg)
        (sdata arg)
      ]
      [(data ,a ,b)
        (data a b)
      ]
      [(stext ,arg)
        (stext arg)
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
        ;;(log-debug "reg=>~a" reg)
        reg
      ]
      [(label ,var)
        (label var)
      ]
      [(proc ,var ,args ...)
        (proc var args)
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
       [(lret)
        (lret)
      ]
      [(cmp-jmp ,a ,b ,c)
        (cmp-jmp a b c)
      ]
      [(cmp-jmp ,val1 ,val2 ,l1 ,l2)
        (cmp-jmp val1 val2 l1 l2)
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
      [(mset ,a ,b ,x ...)
        (mset a b x)
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
      [(asm ,e )
        (note "$asm ~a" e)
        (asm e)
      ]
      [($asm ,args ...)
        (note "$asm ~a" args )
        (mapc  (lambda (e)
            (emit-p e env)) args )
      ]
      ;;prim call 
      [(,app ,args ...)
        (guard (prim? app))
        (note "prim call ~a" app)
        (emit-prim app (map  (lambda (e)
            (emit-p e env)) args ))        
      ]
      [(fcall ,app ,args ...)
        ; (arg (emit-p args env))
        ;(log-debug "call app= ~a ~a" app args)
        ;;call name
        (apply fcall app 
          (map  (lambda (e)
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
        ;(log-debug "call app= ~a ~a" app args)
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
  (log-info "asm-gen=>")
  (pretty-print exp)
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
    ;;(log-debug "out-asm \n~a" out-asm)
    out-asm)  
  )

  (define (asm-gen-file exp name)
    (let ([o (open-file-output-port (format "~a~a" name (option-get 'obj.name ".s") ) (file-options replace)
                (buffer-mode block) (current-transcoder))])
          (display (asm-gen exp) o)  
      (close-output-port o))
    (asm-compile-exp exp name)
  )  

)