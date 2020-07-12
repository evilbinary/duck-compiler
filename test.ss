;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (test)
  (export test add-test-case add-test 
   test-all
   add-test-string
   add-test-print
   )

  (import (scheme) (duck) (logger) (common) (trace) )


(define all-tests (list))
  


(define shell
        (lambda (s . args)
          (system (apply format s args))))

(define (run-shell cmd)
    (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports
                      cmd
                      (buffer-mode block)
                      (native-transcoder))])
        (close-output-port to-stdin)
        (let ([out (get-string-all from-stdout)]
              [err (get-string-all from-stderr)])
          (close-input-port from-stdout)
          (close-input-port from-stderr)
          (unless (eof-object? err) (error 'test err))
          (if (eof-object? out) (set! out ""))
          out)
    ))

(define (test case)
    (let ((name (car case))
          (exps (cadr case))
          (cmp (if (procedure? (caddr case)) (caddr case) equal? ))
          (total 0)
          (passed 0)
          (failed 0)
          (compile-wrong 0)
          )
        (set! exps (map (lambda (d)
          (if (pair? d)
            d
            (list d d)) ) exps))
        (printf "test ~a\n" name)
        (let loop ((exp exps) (i 0))
            (if (pair? exp)
              (let* ((e (car exp))
                      (ret '())
                      (out '())
                    (result  (cdr e))
                    (test-name (format "~a.~a" (string-replace #\space #\. name) i))
                    )
                  (set! total (+ total 1))
                  ;;(printf "   exp=~a\n"   (cadar e) ) ;;(cadadr e ) 
                  (if (pair? e)
                      (set! result (cadr e))
                      )
                  (set! ret (duck-compile-exp (car e) test-name))
                  (printf "[test~a]:" i  )
                  (pretty-print  (car e ) )
                  ;;(pretty-print ret )
                  (if (= ret 0)
                    (begin
                      (set! out (run-shell (format "./~a" test-name)))
                      (if (cmp out result)
                        (begin 
                          (set! passed (+ passed 1))
                          (printf " ==>passed ~a=~a\n" out result)
                        )
                        (begin
                          (set! failed (+ failed 1))
                          (printf  "  ==>failed ~a=~a\n" out result)))
                    )
                    (begin 
                      (set! compile-wrong (+ compile-wrong 1))
                      (printf " ==>compile erro ~a ./~a.s\n" ret test-name)
                      (error 'compile test-name (car e ) )
                      )
                      
                    )
                    
                  (loop (cdr exp) (+ i 1))
              )
            )
        )
        (printf "\n")
      (list total passed failed compile-wrong)
    ))

(define (test-all)
  (let ((total 0)
        (passed 0)
        (failed 0)
        (compile-wrong 0)
        (test-result (map test all-tests) )
      )
      (let loop ([result test-result])
            (if (pair? result)
                (let ((ret (car result))) 
                  (set! total (+ total (list-ref ret 0)))
                  (set! passed (+ passed (list-ref ret 1)))
                  (set! failed (+ failed (list-ref ret 2)))
                  (set! compile-wrong (+ compile-wrong (list-ref ret 3)))
                  (loop (cdr result) ))))
    (printf "test-all total:~a passed:~a failed:~a compile erro:~a\n" total  passed failed compile-wrong)
  ))

(define (add-test-case name exps cmp)
  (set! all-tests 
    (append! all-tests (list (list name exps cmp) ))) )

(define-syntax add-test
  (lambda (x)
    (syntax-case x ()
      [(_ name sexps ... )
        ;;#'(printf "====>~a\n" (list ''sexps ...) )
        #'(add-test-case name  '( sexps ...) '())
      ]
    )))

(define (conver-to-string a)
  (type-case a
      [(symbol?) (symbol->string a)]
      [(number?) (number->string a)]
      [(string?) a]
      [(boolean?) (if a "#t" "#f")]
      [(null?) "()"]
      [(void?) "(void)"]
      [else 
        (with-output-to-string
              (lambda ()  (printf "~a" a )))
                  ])
)

(define (string-cmp a b)
  (set! a (conver-to-string a))
  (set! b (conver-to-string b))
  (string-ci=? a b)
)



(define-syntax add-test-print
  (lambda (x)
    (define (pp exp)
      (let ((d (syntax->datum exp)))
      ; (printf "pp=>~a ~a\n"  d (pair? d))
      (if (not (pair? d))
        (set! d (list d d))
      )
      (printf "d==>~a=~a\n" d (car d) )
      ; (set! d (datum->syntax  #'k d))
      ; #`((printf "%s" #,(car d)) #,(cadr d) )
      ; #`( (printf "%s"  '#,(car '#,@exp) ) 10)
      ; (datum->syntax  #'k d) ;; #,@exp
      ; (datum->syntax  #'k d)
      (if (or (equal? (car d) 'quote ) (equal? (car d) 'void ) )
        (with-syntax ((e (datum->syntax #'k d ))
                    (e2 (datum->syntax #'k  d)))
        (syntax
          ((print-value e) e2)
        ))
        (with-syntax ((e (datum->syntax #'k (car d) ))
                    (e2 (datum->syntax #'k (cadr d) )) )
          (syntax
            ((print-value e) e2)
          ))
      )
      
    ))

    (syntax-case x ()
      [(_ name sexps ... )
        #`(add-test-case name '#,(map pp #'(sexps ...) )  string-cmp )
      ]
    )))

(define-syntax add-test-string
  (lambda (x)
    (syntax-case x ()
      [(_ name sexps ... )
        ;;#'(printf "====>~a\n" (list ''sexps ...) )
        #'(add-test-case name  '( sexps ...) string-cmp )
      ]
    )))

)