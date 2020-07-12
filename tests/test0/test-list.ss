(define make-matrix
    (lambda (name rnum cnum)
        ;;(hashtable-set! @@@@mt name (list rnum cnum))
        (set-box! name (make-bytevector (* rnum cnum 8)))))

(define m (box '()))


(define make-matrix2
    (lambda (name rnum cnum)
        ; (hashtable-set! @@@@mt name (list rnum cnum))
        (set-box! name (make-vector (* rnum cnum)))))


 (define make-list2
    (case-lambda
      [(n) (make-list n (if #f #f))]
      [(n v) (let loop ([n n] [ls '()])
               (if (zero? n)
                   ls
                   (loop (fx- n 1) (weak-cons v ls))))]))

(define make-matrix3
    (lambda (name rnum cnum)
        ;;(hashtable-set! @@@@mt name (list rnum cnum))
        (make-list2 (* rnum cnum ) )))

(define make-matrix4
    (lambda (name rnum cnum)
        ;;(hashtable-set! @@@@mt name (list rnum cnum))
        (make-list (* rnum cnum ) )))

; (collect-request-handler (lambda () 
;     ;(printf "request\n" )
;     #f
; ))

; (collect-trip-bytes (* 10000 10000 10000) )
; (collect-generation-radix 1)
(heap-reserve-ratio 1600000000)
; (collect-maximum-generation 254)

(collect-request-handler void)
; (collect-request-handler (lambda() (collect)))

; (time (make-list2 (* 10000 10000 )))



(define rand
    (lambda ()
        (* (random 101) 0.01)))
(define make-matrix-rand
    (lambda (name rnum cnum)
        ;;(hashtable-set! @@@@mt name (list rnum cnum))
        (let loop ([i 0][end (* rnum cnum)][Res '()])
            (cond
                ((eq? i end) (set-box! name (list->vector Res)))
                (else (loop (+ 1 i) end (cons (rand) Res)))))))


(define n (box 0))
; (time (make-matrix-rand n 5000 5000) )


; (compile-profile #t)

(parameterize ([compile-profile 'source])
  (load "./rand.ss") )
(profile-dump-html "rand")

; (printf "==>~a\n" (profile-dump))
; (printf "===>~a\n" (profile-dump-list) )



;
;  (profile-dump-html)
; (profile-dump-list)
; (let ()
;     (compile-profile #t)
;     (time (make-matrix-rand n 5000 5000) )
;     ; (profile-dump-html)
;     (profile-dump-list)
; )

; (time
;     (let loop ([i 0])
;         (when (< i 5) (make-matrix m 10000 10000) (loop (+ 1 i )) )))

; (time (make-matrix m 10000 10000))
; (time (make-matrix2 m 10000 10000))
; (time (make-matrix3 m 10000 10000))
; (time (make-matrix4 m 10000 10000))