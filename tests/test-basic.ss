;; test
(import (scheme) (test) (duck) )

(add-test-print "test basic"
    0
    -1
    -0
    -5
    5
    1000
    #t
    #f
    ['() ()]
    [(void) ()]
    [(begin (+ 1 2)) 3]
    "test"
    ; "哈哈"

    ['100 100]
    ['#f #f]

    ['(1 2) (1 2)]
    ; ['a a]
    ; ['(a) (a)]
    ; ['(a b c) (a b c)]
    ; [(quote haha) haha]
    ; [(quote (quote a)) "(quote a)"]
    ; [''a "(quote a)"]
    ; ['((A) B C) ((A) B C)]
    ; ['(3 4) (3 4)]
    ; ['(3 . 4) (3 . 4)]
    ; ['(((1 . 2) (3 . 4)) (5 . 6)) (((1 . 2) (3 . 4)) (5 . 6))]
    ; ['((0 . #t) (1 . #f) (3 . #t)) ((0 . #t) (1 . #f) (3 . #t))]
    ; '"abc"
    ; ['(+ 1 2) (+ 1 2)]

    ; ; '#(1 2 3)
    ; ; '#((2 . 3) #f #t (#t . 4))

    [(null? '()) #t]
    [(null? 1) #f]
    [(cdr '(1 . 2)) 2]
    [(pair? '(1 . 2)) #t]
    [(pair? (cons 1 2)) #t]
    [(car (cons 1 2)) 1]
    [(cons 1 2) (1 . 2)]

    [(cdr (cons 1 2)) 2]
    [(null? (cdr (cons 1 '()))) #t]

    [(car '(1 . 2)) 1]
    ['(1 2) (1 2)]
 

    ; (vector-ref '#(1 2 3) 1)
    ; (vector-length '#(1 2 3))
    ; (vector-length (make-vector 10))
  )


(test-all)
