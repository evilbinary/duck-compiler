;; test
(import (scheme) (test) (duck) )

(add-test-print "test if"     
    [(if #f 1 2) 2]
    [(if #t 1 2) 1]
    [(if #f #f) "()"]

    [(if (> 3 2) (- 3 2) (+ 3 2)) 1]
    [(if (+ 1 2) 1 2) 1]
    [(if (< 3 2) (+ 3 4) (+ 4 5)) 9]

    [(if (= 1 1)
            0
            (if (= 2 2)
                1
                2
                )
            ) 0]

  ;;no pass
      ; [(if (> 2 3) 'yes 'no) no]
    ; [(if (> 3 2) 'yes 'no)  yes]
   
  )


(test-all)
