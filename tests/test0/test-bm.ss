(define (compute-last-occ p)
  (let ((last-occ (make-vector 128 -1)))
    (let loop  ((len (- (string-length p) 1))
                (i 0))
      (if (< i len)
        (begin
          (vector-set! last-occ (char->integer (string-ref p i)) i)
        ;   (printf "char=~a int=~a i=~a\n" 
        ;     (string-ref p i) 
        ;     (char->integer (string-ref p i))
        ;     i
        ;    )
          ;(printf "==>~a\n" last-occ )
          (loop len (+ i 1))
        )
      )
    )
    last-occ
  ))

(define (bmh t p)
  (let ((last-occ (compute-last-occ p))
        (i0 0)
        (n (string-length t))
        (m (string-length p))
        (j -1)
        (ret -1)
        (is-not-ret #t)
      )
    ; (printf "vec=>~a" last-occ)
    ; (print-last-occ last-occ)
    (let loop ()
      (if (< i0 (- n m))
        (begin
          (set! j (- m 1))
        ;   (printf "i0=~a j=~a\n" i0 j)
          (let loop2 ()
            (if (char=? (string-ref p j) (string-ref t (+ i0 j)) )
              (begin 
                  (set! j (- j 1))
                  (if (< j 0)
                    (begin 
                      (set! is-not-ret #f)
                    ;   (printf "ret====>~a\n" i0)
                      (set! ret i0) )
                    (loop2)
                  ))))
          (if is-not-ret
            (begin
            (set! i0 (+ i0
                      (- m 1)
                      (-  (vector-ref last-occ 
                            (char->integer (string-ref t (+ i0 m -1))) )  )  ))
            ; (printf "i0=~a val=>~a char=>~a\n" 
            ;     i0  
            ;     (vector-ref last-occ
            ;                 (char->integer (string-ref t (+ i0 m -1))) )
            ;     (string-ref t (+ i0 m -1)) )
            (loop)))
        )))
    ret))

(define (print-last-occ last-occ)
    (let loop ((i (char->integer #\a)) (j 0) )
        (if (<= i (char->integer #\z))
            (begin 
                (printf "~a ~a;" (integer->char i) (vector-ref last-occ i))
                (set! j (+ j 1))
                (if (= 0 (/ j 13))
                    (newline)
                )
                (loop (+ i  1) j)
            )
        )
    )
)

(let ((l (compute-last-occ "abacbb" ) ))
    (print-last-occ l)
    (printf "b=>~a\n" (vector-ref l (char->integer #\b)))

    ;(printf "last-occ =>~a" l)
)
(bmh "abacaxbaccabacbbaabb" "abacbb")