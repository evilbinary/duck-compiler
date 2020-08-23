;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (duck)
  (export duck-compile
  duck-compile-exp
  ast-conversion
  )

(import (scheme)
        (match)
        (egg)
        (trace)
        (common)
        )

(define (print-exp exp)
  (pretty-print exp)
  exp)

(define (clear-gen-symbol exp)
  (clear-sym)
  exp
)  

(define (c-main) "
  int main(int argc, char** argv){
    return 0;
  }
")

(define (macro-conversion exp)
  exp
)


(define (ast-conversion exp)
(printf "exp ~a\n" exp)
(match exp
  [(define (,v ,e ,e* ...) ,e1)
    (ast-conversion `(define ,v (lambda (,e ,@e*) ,e1)) )
  ]
  [(define ,v (lambda (,args ...) ,body ))
    (ast-conversion 
    `(let ((,v  (lambda (,args ...) ,body ) )) ,v )
        )
  ]
  [(define ,v ,e)
    (printf "define ~a ~a\n" v e)
    ;;`(set! ,v ,(ast-conversion e) )
    (ast-conversion 
    `(let ((,v ,e )) ,v )
        )
    ]
  ; [(begin ,e1 ,e2) 
  ;   `(let ((,e1))
  ;       ,(ast-conversion e2))
  ; ]
  ; [(begin ,e1 ())
  ;   `(begin ,e1)
  ; ]
  [(begin . ,e1 )
    (printf "begin ~a len=~a\n" e1 (length e1))
    (if (null? e1)
    '()
    (if (> (length  e1) 1 )
      `(let ([,(gen-sym 'var) ,(ast-conversion (car e1) ) ] )
            ,(ast-conversion `(begin ,@(cdr e1) ) )
        )
        (ast-conversion (car e1))
        ))
  ]
  [(let ((,e1 ,e2) ,e* ...) ,e3* ...)
    (printf "let->~a , ~a\n" e1 e3* )
    (if (null? e*)
      (if (null? e3*)
        `(let ((,e1 ,(ast-conversion e2) )) (void) ) ;;syntax erro
        `(let ((,e1 ,(ast-conversion e2) )) ,(ast-conversion `(begin ,@e3* ) ) )
        )
      `(let ((,e1 ,(ast-conversion e2) ))
        ,(ast-conversion `(let ,e* ,@(ast-conversion e3* )  ) ) )
      )
  ]
  [(let () ,e1 ...)
    (ast-conversion `(begin ,@e1) )
  ]
  [(let ,e1 () )
    `(let ,e1 )
  ]
  [(let ,e ([,e1* ,e2*] ...) ,e3* ...)
    (printf ">>>>>>>>>>>>>>>>let ===~a ~a\n" e1* e2*)
    (ast-conversion `(let ((,e (lambda ,e1*  ,@e3* ) ))
        (,e ,@e2*)
      ))
  ]
  [(lambda (,e1 ...) ,e2 ...)
    (printf "lambda====>~a\n" e2)
    `(lambda ,e1 ,(ast-conversion `(begin ,@e2 ) ) )
  ]
  [(if ,e1 ,e2 ,e3)
    (printf "if====>\n")
    `(if ,(ast-conversion e1) ,(ast-conversion e2) ,(ast-conversion e3))
  ]
  [(if ,e1 ,e2)
    (printf "if1====>\n")
    `(if ,(ast-conversion e1) ,(ast-conversion e2) (void) )
  ]
  [(if ,e1 )
    (printf "if2====>\n")
    `(if ,(ast-conversion e1) (void) (void) )
  ]
  ; [((,e)) `(,e)]
  [(()) '()]
  [(quote ,e)
    (printf "quote ~a\n" exp)
    (cond
      [(pair? e) `(cons ,(ast-conversion `(quote  ,(car e))) ,(ast-conversion `(quote ,(cdr e))) ) ]
      [else `(quote ,e) ]
    )
  ]
  [(,app ,arg)
    (printf "app ~a\n" exp)
    `(,app ,(ast-conversion arg))
  ]
  [(,binop ,e1 ,e2 ...)
    (guard (memq binop '(+ - * /) ))
    (printf "prim ~a ee=>~a\n" binop e2)
    (if (> (length e2) 1)
    `(,binop ,(ast-conversion e1) ,(ast-conversion `(,binop ,@e2 )) )
    `(,binop ,(ast-conversion e1) ,(ast-conversion (car e2) ) )
      )
  ]
  [(,app ,e1 ...)
    `(,app ,@(map ast-conversion e1))
  ]
  [,exp 
    (printf "other exp->~a\n" exp)
    exp
  ]
  )
)

(define (alpha-conversion exp)
    (define (alpha-conv exp env)
      (match exp
      [(define ,v ,e)
        `(set! ,v ,e)
        ]
      [,exp exp]
      )
    )
    (alpha-conv exp '())
  )

(define (closure-conversion exp)
  exp
)

(define (cps-conversion exp)
  exp
)

(define (type? v)
  (or (number? v)
      (symbol? v)
      (boolean? v)
      ; (string? v)
      (null? v)
      (void? v)
      ; (equal? 'quote (car v))
      )
)

(define (anf-term exp) (anf exp id))

(define (anf-name exp k)
  (printf "=======>anf-name ~a type=>\n" exp )
  (anf exp (lambda (n)
    (if (type? n)
      (begin 
        ; (printf "=======>anf-name type->ret=~a  ~a\n" (k n)   n)
        (k n)
      )
      (let ([t (gen-sym 'var)])
        (printf "=======>gen->~a\n" t)
        `(let ([,t ,n]) ,(k t) )
      )
    )
  ))
)

(define (anf-name* exp* k)
  (printf "=======>anf-name* ~a\n" exp* )
  (if (null? exp*)
    (begin 
      (printf "----------->exps end exp*=~a\n" exp*)
      (k exp*)
    )
     (anf-name (car exp*) 
      (lambda (t)
        (printf "=========>(car exp*)=~a ~a\n" (car exp*) t)
        (anf-name* (cdr exp*) (lambda (t*) 
        (printf "=======>anf-name3 ===>t*=~a\n" t*)
        (k `(,t . ,t*)))) ))
  )
)

(define (anf exp k)
  (printf "anf exp ~a\n" exp)
  (match exp
    [,v ;;atom
      (guard (type? v))
      (k v)]
    [(lambda (,params ...) ,body)
      (k `(lambda ,params ,(anf-term body)))
    ]
    [($asm ,args ...)
      (k `($asm ,@args))
    ]
    [(if ,e1 ,e2 ,e3)
      (printf "if->~a ~a ~a k=~a\n" e1 e2 e3 k)
      ; `(if ,(anf e1 id) ,(anf e2 k) ,(anf e3 k) )
      (k `(if ,(anf e1 id) ,(anf e2 id) ,(anf e3 id)) )
    ]
    [(let ((,v ,exp1) ) ,exp3)
      (printf "let->~a ~a ~a\n" v exp1 exp3)
      (anf exp1 
        (lambda (aexp1)
          `(let ([,v ,aexp1]  )
              ,(anf exp3 k) )))
    ]
    ; [(set! ,e1 ,e2 )
    ;   `(set! ,e1 ,(anf e2 k) )
    ; ]
    [(()) '()]
    [(void) (k `(void) ) ]
    [(quote ,e)
      (printf "quote-> ~a ~a\n" e `(quote ,e ))
      ; `(quote ,(anf e k) )
      (k `(quote ,e))
    ]
    [(,fn . ,e*)
      (printf "fn->~a arg->~a\n" fn e*)
      (anf-name fn (lambda (t) 
        (printf "=======>anf-name2 ~a\n" t)
        (let ((ret (anf-name* e* (lambda (t*)
              (k `(,t . ,t*)))) ))
          (printf "=======>anf-name2 ret ~a\n" ret)
          ret
        )
        
        ))
      ]
    [,?
    (printf "?->~a\n" (k ?) )
    (k ?)]
  )
)

(define (anf-conversion exp )
  (anf exp id)
)

(define (mark-tail exp fn)
  (printf "mark-tail->~a\n" exp)
  (match exp
    [(lambda ,e1 ,e2)
      `(lambda ,e1 ,(mark-tail e2 fn) )
    ]
    [(let ,e1 ,e2)
      `(let ,e1 ,(mark-tail e2 fn) )
    ]
    [(if ,e1 ,e2 ,e3)
      `(if ,(mark-tail e1 fn) ,(mark-tail e2 fn) ,(mark-tail e3 fn))
    ]
    [(,app ,args ...)
      (if (equal? fn app)
      `(tail ,app ,@args)
      exp
      )
    ]
    [,exp 
      (printf "exp->~a\n" exp)
      exp]
  )
)

(define (tail-convert e)
  (define (T-ae ae)
    (printf "tail ~a\n" ae)
    (match ae
      [,x
        (guard (type? x))
       x]
      [(lambda (,x) ,e0)
       (define k (gen-sym 'k))
       `(lambda (,k ,x) ,(T-e e0 k))]
      [else ae]))
  (define (T-e e cae)
  (printf "tail T-e ~a\n" e)
    (match e
      [,x
        (guard (symbol? x))
       `(,cae ,x ,x)]
      [(lambda . ,rest)
       `(,cae 0 ,(T-ae e))]
       [(let ([,x ,e0]) ,e1)
       (define _x (gen-sym 'k))
        (T-e e0 `(lambda (,_x ,x) ,(T-e e1 cae)))]
      ; [(let ([,x ,e0]) ,e1)
      ;  (define _x (gen-sym 'k))
      ;  `(let (( ,(T-e e0 `(lambda (,_x ,x) ,(T-e e1 cae))) )))
      ;  ]
      [(if ,ae ,e0 ,e1)
       `(if ,ae ,(T-e e0 cae) ,(T-e e1 cae))]
      [(,aef ,aes ...)
        (if (equal? cae '())
        `(,(T-ae aef) ,@(map T-ae aes))
        `(,(T-ae aef) ,cae ,@(map T-ae aes))
        )
       ]
       [else e]
       ))
  (T-e e `() )

    ; (printf ">>>>>tail convert== ~a\n" e)
    ; (match e
    ;   [(let ([,e0 (,e1 ,app)]) ,e2)
    ;     (printf ">>>>>tail convert let2 = (~a ~a)\n" e0 e1 )

    ;     (let ((k (gen-sym 'k)))
    ;     `(let ([,k (lambda (,e0) ,(tail-convert e2) ) ])
    ;       (,k ,(tail-convert `(,e1 ,app) ))
    ;       ))
    ;   ]
    ;   [(let ([,e0 ,e1]) ,e2)
    ;   (printf ">>>>>tail convert let = (~a ~a)\n" e0 e1 )
    ;   ;  (if (equal? (car e1) '( ))
    ;   ;   (let ((k (gen-sym 'k)))
    ;   ;   `(let ([,k (lambda (,e0) ,(tail-convert e2) ) ])
    ;   ;     (,k ,(tail-convert e1) )
    ;   ;     ))
    ;     `(let ([,e0 ,(tail-convert e1) ])
    ;         ,(tail-convert e2)
    ;       )
    ;       ; )
    ;   ]
      
    ;   [(if ,e1 ,e2 ,e3)
    ;     `(if ,e1 ,(tail-convert e2) ,(tail-convert e3) )
    ;   ]
    ;   [(lambda ,e1 ,e2)
    ;     `(lambda ,e1 ,(tail-convert e2 ) )
    ;   ]
    ;   [,e e]
    ;   )

  )

(define (tail exp)
  (match exp
    [(let ((,v ,exp1)) ,exp2)
        ;;(printf "==>tail ~a ,~a\n" v exp1)
        (if (and (pair? exp1) (equal? (car exp1) 'lambda))
             ;;mark tail
             (begin
             (printf "==>mark tail ~a\n" (mark-tail exp1 v))
             `(let ((,v ,(mark-tail exp1 v) )) ,(tail exp2) )
            ;  (tail-convert `(let ((,v ,(mark-tail exp1 v) )) ,(tail exp2) ) )

             )
        `(let ((,v ,(tail exp1) )) ,(tail exp2) )
      )
    ]
    [,exp 
    (printf "exp->~a\n" exp)
    exp]
  )
)

(define (tail-conversion exp)
  (tail exp)
)

(define (compile-steps exps steps)
   (let loop ((it steps) (exp exps) (i 0))
      (if (pair? it)
        (let ((ret '()))
          (printf "~a.step ~a\n" i (car it) )
          ;;(printf "   exp =~a\n" exp)
          (printf " exp=>")
          (pretty-print exp)
          ;;(printf "symbol ~a\n"   (car it))
          (if (pair? (car it))
            (set! ret (apply (car (car it)) exp (cdr (car it)) ))
            (set! ret ((car it) exp)))
          ;;(printf "   ret =~a\n\n" ret)
          (printf " ret=>")
          (pretty-print ret)
          (loop (cdr it) ret (+ i 1)))
        exp
        )
    )
)

(define (duck-compile-exp exp out)
    ;;asm-gen
    (let ((compile-exp (duck-compile exp)))
      (printf " compile=>" )
      (pretty-print compile-exp)
      (printf " gen file=>~a\n" out)
      (asm-gen-file compile-exp out))
)

(define (duck-compile exps)
  (let ((out 'out))
    (clear-gen-symbol '())
    (compile-steps exps (list
      ;;macro-conversion
      ast-conversion
      ;;alpha-conversion
      ;;cps-conversion
      anf-conversion
      ;;tail-convert
      tail-conversion
      ; closure-conversion
      represent-conversion
      flatten-conversion
      instruct-conversion
      instruct-optimize
      assign-conversion
      restruct-block
    ) ))
)

)