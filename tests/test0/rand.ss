; (define x 123456789)
; (define y 362436069)
; (define z 521288629)

; (define (rand x y z )
;     (define t '())
;         (let* 
;             ([x1 (logxor x (logand #xffffffffffffffff (bitwise-arithmetic-shift-left x 16))) ]
;             [x2 (logxor x1 (bitwise-arithmetic-shift-right x1 5))]
;             [x3 (logxor x2 (logand #xffffffffffffffff  (bitwise-arithmetic-shift-left x2 1))) ])
;             (set! t x3)
;             (set! x y)
;             (set! y z)
;             (set! z (logxor t x y))
;             z))

; (define (rand-init x y z)
;  (define t '())
;  (lambda () 
;     (let* (
;   (x1 (logxor x (logand #xffffffffffffffff (bitwise-arithmetic-shift-left x 16))))
;   (x2 (logxor x1 (ash x1 -5)))
;   (x3 (logxor x2 (logand #xffffffffffffffff (bitwise-arithmetic-shift-left x2 1))))

;     )
;   (set! t x3)
;   (set! x y)
;   (set! y z)
;   (set! z (logxor t x y))
;   z))
; )

; (define rand-maker (rand-init 123456789 362436069 521288629))

(define gseed 1)

(define (rand-int)
    (set! gseed (+ 2531011 (* gseed 214013 ) ))
    (logand #xffffffffffffffff (ash gseed 16))
)

 

(time (let loop ((n 25000000))
    (if (> n 0)
        (let ((r (rand-int))) 
        (printf "rand ~a\n"  r  )
        (loop (- n 1))
        )
    )
))
 