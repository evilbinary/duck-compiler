;; test
(import (scheme) (test) (duck) )

(add-test-print "test let"
  [(let ((a 10)) ) ()]
  [(let ((a 10) (b 11) (c 12) (d 13)) ) ()]
  [(let ((a 10) (b 11) (c 12) (d 13)) 1) 1]
  [(begin 1 (begin 11 22) ) 22]
  [(let ((a 10) (b 11) (c 12) (d 13)) 1 2 3 4 (let () 5 6 )) 6]


  [(let ([var.18 "=>0 "])
        (let ([var.0 (printc var.18)]) 1))
    "=>0 1"
  ]

  ; no pass
  [(let ((idx (lambda (x) x))
          (a 10)
          (b 11)
          (c 12))
        (+ a b)
      )
      21]
   [(let loop2 ([x 10] [y 11] )
        (if (> x 0)
          (begin 
            (printc "%d,%d " x y)
            (loop2 (- x 1) y)))
        )
      "10,11 9,11 8,11 7,11 6,11 5,11 4,11 3,11 2,11 1,11 "
    ]

  ; [(let ((id (lambda (x) x)))
  ;   (let ((apply (lambda (f x) (f x))))
  ;     ((id apply) (id 3))))
  ;      '()]



   
  )


(test-all)
