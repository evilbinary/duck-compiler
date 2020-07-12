;; test
(import (scheme) (test) (duck) )

(add-test-string "test var"
  ; [x 'x]
  [1000 1000]
  ["test" "test"]
  [#t #t]
  [#t #f]
  [#f #f]
  ;[(compile '1000) 1000]
  )

(test-all)
