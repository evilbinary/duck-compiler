;; test
(import (scheme) (test) (duck) )

(add-test-print "test cmp"     
    [(> 2 3) #f]
    [(> 3 2) #t]
    [(= 3 2) #f]
    [(= 2 2) #t]
    [(< 2 3) #t]
    [(< 3 2) #f]
    [(= 0 0) #t]
    [(> 0 0) #f]
    [ (begin 
      (<= 1 0)
      (= 0 0)
      ) #t]
    [(= 0 -1) #f]
    [(= -1 -1) #t]
  )

(test-all)
