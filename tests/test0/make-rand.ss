(collect-request-handler void)

(define rand
    (lambda ()
         (random 1.0)))


(define (extend! l . xs)
  (if (null? (cdr l))
      (set-cdr! l xs)
      (apply extend! (cdr l) xs)))

(define make-matrix-rand
    (lambda (name rnum cnum)
        ;;(hashtable-set! @@@@mt name (list rnum cnum))
        (let loop ([i 0][end (* rnum cnum)][Res '() ])
            (cond
                ((eq? i end) (set-box! name (list->vector Res)))
                (else 
                     ;;(rand)
                    (loop (+ 1 i) 
                        end 
                        (cons (rand) Res )
                        ))))))


(define n (box 0))
(time (make-matrix-rand n 5000 5000))