;; test
(import (scheme) (test) (duck) )


; (add-test "test tail call"
;     [(let g (fix f (lambda (n a) (if (zero? n) a (f (- n 1) (* n a)))))
;        (g 10 1)) '()]
; )


(add-test "test tail call conversion"
;   [(let ([a (fib 10)])
;       (+ 1 a))
;   '(fib (lambda (a)
;     (+ 1 a)
;    ) 10)
      
;   ]

;;test normal tail call
[(begin
    (define aa (lambda (a) a))
    (define f (lambda (n) 
        (printc "f(%d) " n)
        (if (= n 0)
            0
            (f (- n 1))
        )
     ))
    (printc "%d" (f 100))
    )
'0
]

[(let ((f (lambda (n) 
        (printc "f(%d) " n)
        (if (= n 0)
            -1
            (f (- n 1))
        )
     )))
    (printc "%d" (f 100))
    )
'-1
]

;;test fib iter tail call ok
[(let ((fib (lambda (n a b)
                (if (<= n 1)
                    b
                   (fib (- n 1) b (+ a  b)  )
                )
                 )))
            (printc "%d" (fib 30 0 1))
        )
    '832040
]

; [(begin 
;   (define Y 
;     (lambda (f)
;         ((lambda (x) (x x))
;         (lambda (x) (f (lambda (y) ((x x) y)))))))

; ;   (define fib
; ;     (Y (lambda (f)
; ;         (lambda (x)
; ;             (if (< x 2)
; ;                 x
; ;                 (+ (f (- x 1)) (f (- x 2))))))))

;  (define fib
;     (lambda (x)
;         ((Y (lambda (f)
;             (lambda (n a b)
;                 (if (<= n 1)
;                     b
;                     (f (- n 1) b (+ a b))))))
;         x 0 1)))

;   (printc "%d" (fib 35))
; )
; '9227465]

[(define fac (lambda (n)
        (if (< n 2 )
            1
            (* n (fac (- n 1)))
        )
    ))

'()]


  [(define fib (lambda (n)
        (if (<= n 1)
            n
            (+ (fib (- n 1)) (fib (- n 2))))))
            
    '(letrec ((fib (lambda (n a b)
                (if (<= n 1)
                    b
                   (fib (- n 1) b (+ a  b)  )
                )
                 )))
            (fib 6 0 1)
        )
     ]

; [(k.0 (fib var.1))
;     '()
; ]

 [(begin 
    (define fib (lambda (n)
        (if (<= n 1)
            n
            (+ (fib (- n 1)) (+ 1 2) (fib (- n 2)))))) 
    (fib 10))
  6765 ]

;tail call no if
   [(begin 
    (define fib (lambda (n)
            (+ (fib (- n 1)) (+ 1 2) (fib (- n 2)))))
    (fib 10))
  6765 ]

)

(test-all)
