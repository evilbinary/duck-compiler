;; test
(import (scheme) (options))
(option-set 'arch 'wasm)
(import  (test) (duck) )


(add-test-print "test var"
  ; [x 'x]
  ; [1000 1000]
  ["Hello, world!" "Hello, world!"]
  ; [#t #t]
  ; [#t #f]
  ; [#f #f]
  ;[(compile '1000) 1000]
  )

(test-all)
