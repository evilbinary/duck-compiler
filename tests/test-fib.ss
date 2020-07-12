;; test
(import (scheme) (test) (duck) )

(add-test-string "test fib"
    ; [(begin
    ;     (define f (lambda (n)
    ;         (printc "f(%d)" n)
    ;         (if (> n 0)
    ;             (f (- n 1))
    ;             n
    ;             )
    ;         )
    ;     )
    ;     (f 10)
    ;     )
    ; "f(10)f(9)f(8)f(7)f(6)f(5)f(4)f(3)f(2)f(1)f(0)"
    ; ]
    ; [(begin
    ;     (define f (lambda (n)
    ;         (printc "f(%d)" n)
    ;         (if (> n 0)
    ;             (if (= n 1)
    ;                 1
    ;                 (if (= n 2)
    ;                     2
    ;                     (f (- n 1)) )
    ;             )
    ;             0
    ;         )
    ;     ))
    ;     (printc "%d" (f 10))
    ;     )
    ; "f(10)f(9)f(8)f(7)f(6)f(5)f(4)f(3)f(2)2"
    ; ]
    ; [(begin 
    ;     (define fib (lambda (n)
    ;         (if (<= n 1)
    ;             n
    ;             (+ ( fib (- n 1)) (fib  (- n 2))) )
    ;     ))
    ;     (print-value (fib 10)))
    ; 55 ]

    ; [(begin 
    ;     (define fib (lambda (n)
    ;         (if (= n 0)
    ;             0
    ;             (if (= n 1)
    ;                1
    ;                 (+ ( fib (- n 1)) (fib  (- n 2))) ))
    ;     ))
    ;     (print-value (fib 10)))
    ; 55 ]

    [(begin 
        (define fib (lambda (n)
            (printc "f(%d) " n)
            (if (<= n 0)
                (begin 
                    (printc "=>0 ")
                    0
                    )
                (if (= n 1)
                    (begin 
                        (printc "=>1 " n)
                        1
                    )
                    (let ((r (+ (fib (- n 1)) (fib (- n 2))) )) 
                        (printc "=>%d " r)
                        r
                    )
                )
                )
        ))
        (printc "ret=%d" (fib 10)))
    ; "f(10) f(9) f(8) f(7) f(6) f(5) f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 =>5 f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 =>8 f(5) f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 =>5 =>13 f(6) f(5) f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 =>5 f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 =>8 =>21 f(7) f(6) f(5) f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 =>5 f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 =>8 f(5) f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 =>5 =>13 =>34 f(8) f(7) f(6) f(5) f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 =>5 f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 =>8 f(5) f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 =>5 =>13 f(6) f(5) f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 =>5 f(4) f(3) f(2) f(1) =>1 f(0) =>0 =>1 f(1) =>1 =>2 f(2) f(1) =>1 f(0) =>0 =>1 =>3 =>8 =>21 =>55 ret=55" 
    55
    ]

;  [(begin 
;     (define fact
;       (lambda (n)
;         (if (= n 1)
;           1
;           (* n (fact (- n 1))))))
;     (fact 8))
;           40320]

    ; (define fact-iter
    ; (lambda (n acc)
    ;     (if (= n 1)
    ;         acc
    ;         (fact-iter (- n 1) (* n acc)))))

;   [(begin 
;     (define Y
;         (lambda (f)
;             ((lambda (x) (x x))
;             (lambda (x) (f (lambda (y) ((x x) y)))))))


;     (define F-fibonacci
;         (lambda (f)
;             (lambda (n)
;             (if (= n 0)
;                 0
;                 (if
;                     (= n 1)
;                     1
;                     (+ (f (- n 1)) (f (- n 2)))
;                  ))
;             )))


;     (define fibonacci-4 (Y F-fibonacci))
;     (printc "fib=%d" (fibonacci-4 30))
;   )
;     832040
;   ]

)


(test-all)
