;; test
(import (scheme) (test) (duck) )

(add-test "test define"
  [(define a 100) ""]
  [(define square
      (lambda (x)
        (* x x)))
    ""
  ]
  [(define (square x)
        (* x x))
    ""
  ]
  [(begin 
    (define fib (lambda (n)
       ;;(printc "fib[%d] \\n" n)
       (if (= n 0)
         (begin 
            (printc "1 ")
            0
            )
         (if (= n 1)
            (begin 
            (printc "2 ")
            1
            )
            (begin 
                (printc "f2[%d] " n)
                (+ (fib (- n 1)) (fib (- n 2)))
            )
         )
         )
         ))
    (printc "test=%d" (fib 10))
    ) '()]

  ; [(begin
  ;     (define fib (lambda (n) 
  ;         (printc "fib(%d)" n)
  ;         (fib (- n 1))
  ;      ))
  ;     (fib 10)
  ;     )
  ; '()
  ; ]
  
)

; (add-test "test combine"
;   [(set! square (lambda (x) (* x x))) '()]  
;   [(define a (lambda () (a))) '()]
;   [(begin
;         (define (factorial n)
;             (define (iter product counter)
;                 (if (> counter n)
;                 product
;                 (iter (* counter product)
;                     (+ counter 1))))
;             (iter 1 1))
;         (printc "ret=%d" (factorial 30)) )

;             '()]
;   )

(test-all)
