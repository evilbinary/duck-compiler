;; test
(import (scheme) (test) (duck) )

(load "../tests/test-basic.ss")
(load "../tests/test-var.ss")
(load "../tests/test-prim.ss")
(load "../tests/test-print.ss")

(load "../tests/test-cmp.ss")
(load "../tests/test-if.ss")
(load "../tests/test-lambda.ss")
(load "../tests/test-define.ss")
(load "../tests/test-define.ss")
(load "../tests/test-let.ss")
(load "../tests/test-set.ss")
(load "../tests/test-tail-call.ss")

;;;;;;duck-compile test

;;(pretty-print (ast-conversion '(let ((a 1) (b 2) (c 3)) 4 5) ) )

; (duck-compile '(begin
;     (set! square (lambda (x.1) (* x.1 x.1))) (+ (square 5) 1)))

; (duck-compile  'x)
; (duck-compile '(begin
;           (set! a (foo 7))
;           (set! b (cons 1 (bar 3 4))))
;        )

; (duck-compile '1000)
; (duck-compile '(define a "test"))


; (duck-compile '(define square
;   (lambda (x)
;     (* x x))))

; (cps '(lambda (x) (set! x 10) ) id)

; (duck-compile '(lambda (x) (set! x 10) ))
;;(duck-compile '(lambda (x) (* x x) ))

; (duck-compile '(f (if a b c)))
; (duck-compile '(lambda (x) x) )

(test-all)
