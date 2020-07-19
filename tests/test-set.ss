;; test
(import (scheme) (test) (duck) )

(add-test "test set!"
  [(set! v 1000) ""]
  [(set! square (lambda (x) (* x x))) ""]
  )

(test-all)
