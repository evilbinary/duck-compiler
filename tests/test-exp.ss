;; test
(import (scheme) (test) (duck) )

(add-test "test exp"
  [e0 (lambda (x) x) ]
  [(e0 e1) '() ]
  [(e0 e1 e2) '() ]
  )


(test-all)
