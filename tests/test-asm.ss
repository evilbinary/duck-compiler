;; test
(import (scheme) 
  (test)  
)

(add-test-string "test asm"
  [($asm `(block all
            (block main 
              (set reg0 reg1)
              (set reg0 reg1)
              )))   ""]
  [($asm `(set reg0 reg1)) ""]
  
  )

(test-all)
