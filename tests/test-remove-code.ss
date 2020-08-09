;; test
(import (scheme) (test) (duck) (egg) )

(define code `(code
  (code
    (code
      (code
        (= 8
           8))
      (cmp-jmp
        reg0
        false-rep
        ifb.0
        ifa.0)
      (label
        ifa.0)
      (code
        (set reg0 0))
      (jmp ifend.0)
      (label
        ifb.0)
      (code
        (code
          (= 16
             16))
        (cmp-jmp
          reg0
          false-rep
          ifb.1
          ifa.1)
        (label
          ifa.1)
        (code
          (set reg0 8))
        (jmp ifend.1)
        (label
          ifb.1)
        (code
          (set reg0 16))
        (label
          ifend.1))
      (label
        ifend.0))
    (set var.0 reg0))
  (code
    (print-value
      var.0)))
)

;;return demo 
; ( (= 8 8)) 
;    (cmp-jmp reg0 #fse-rep ifb.0 ifa.0)
;    (label ifa.0) (set reg0 0) (jmp ifend.0)
;    (label ifb.0)
;    (= 16 16))
;    (cmp-jmp reg0 #fse-rep ifb.1 ifa.1)
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


; (define (remove-begin e )
;   (match e
;     [(begin (begin ,e1 ... ) ,e2 ...)
;       (remove-begin `(begin ,@e1 ,@e2))
;     ]
;     [(begin ,e1 ... (begin ,e2 ... ) ,e3 ... )
;       (printf "begin ===>\n")
;       `(begin ,@e1 ,@e2 ,@e3)
;     ]
;     [(begin ,apps ...)
;       `(begin ,@(map remove-begin apps))
;     ]
;     [,v v]
;   )
; )
(pretty-format 'code '(_ 0 name 0 args 0 vars  ... ))
(pretty-print  code)

(set! code `(code (set reg0 x)))

(let ((ret (remove-code code)))
  (printf "result=>\n")
  (pretty-print  ret)
)

;;(pretty-print (remove-code code))
