;; test
(import (scheme) (test) (duck) (egg) )

(define code `(code
  (code
    (code
      (code
      (= 8 8))
      (cmp-jmp reg0 false-rep ifb.0 ifa.0)
      (label ifa.0)
      (code (set reg0 0))
      (jmp ifend.0)
      (label ifb.0)
      (code
        (code (= 16 16))
        (cmp-jmp reg0 false-rep ifb.1 ifa.1)
        (label ifa.1)
        (code
          (set reg0 8))
        (jmp ifend.1)
        (label ifb.1)
        (code
          (set reg0 16))
        (label ifend.1))
      (label ifend.0))
    (set var.0 reg0))
  (code
    (print-value var.0)))
)


(define (exist-match-lisp xs) ;exist-match? g xs
  (define (_ xs)
    (if (nilp xs) Fal
      (if (lisp (car xs)) Tru
        [_ (cdr xs)]
  ) ) )
  (_ xs)
)
(define (flat-except-last-layer xs) ;
  (define (_ xs ret)
    (if~
      (nilp xs) ret
      (atomp xs) (cons xs ret)
      (exist-match-lisp xs) ;
        (_ (car xs)
          [_ (cdr xs) ret])
      (cons xs ret)
  ) )
  (_ xs nil)
)
(define (simple x)
  (if~
    (atomp x) x
    (cdr-nilp x) (car x)
    x
) )
(define (asd xs x) ;
  (define (_ xs)
    (if~
      (eq xs x) nil
      (atomp xs) xs
      (let ([a (car xs)] [d (cdr xs)])
        (if~
          (eq a x) (simple [_ d])
          (cons (_ a)
            (_ d)
  ) ) ) ) )
  (flat-except-last-layer (_ xs))
)
(asd code 'code)

;;return demo 
; ( (= 8 8)) 
;    (cmp-jmp reg0 false-rep ifb.0 ifa.0)
;    (label ifa.0) (set reg0 0) (jmp ifend.0)
;    (label ifb.0)
;    (= 16 16))
;    (cmp-jmp reg0 false-rep ifb.1 ifa.1)
;    (label ifa.1)
;    (set reg0 8)
;    (jmp ifend.1)
;    (label ifb.1)
;    (set reg0 16)
;    (label ifend.1)
;    (label ifend.0)
;   (set var.0 reg0)
;   (print-value var.0)
;   )

; (pretty-print (asd code 'code))

;;(pretty-print (remove-code code))
