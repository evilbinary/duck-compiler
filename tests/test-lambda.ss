;; test
(import (scheme) (test) (duck) )

(add-test-print "test lambda"
  [((lambda (x) x) 1) 1]
  [((lambda (x y z) (+ 2 x) ) 100) 102]
  [((lambda (p q r) q ) 1 2 3) 2]
  [(lambda (p1 p2 p3) p3) ""]
  [(lambda (x) x) ""]
  [(begin (define test1 (lambda (x) (+ x 10)) ) 
      (test1 100))
       110]

  ; [((lambda (x) x) 'x) x]
  ; [(lambda (x) 1 2) ""]
  ; [(lambda (x) (lambda (x) 'x) ) ""]  


  ;;no pass
  ; [(define fib (lambda (n) (printc "fib(%d)" n) (if (= n 0) 0 (fib (- n 1))))) ""]
  
  ;;todo support more than 2 args

  )

; (add-test "test alpha conversion"
;   [(lambda (x) x) '()]
;   [(lambda (x y) y) '()]

; )

(test-all)