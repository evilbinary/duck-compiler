;; test
(import (scheme) (test) (duck) )

(add-test-print "test begin"
  [(begin (+ 1 2) (+ 3 4) ) 7]
  [(begin (+ 1 2) (begin (+ 2 3) (+ 3 4)) ) 7]
  )  


(test-all)
