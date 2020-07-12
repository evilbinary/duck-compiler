;; test
(import (scheme) (test) (duck) )


(add-test "test print"
    [(printc "haha") "haha"]
    [(printc "if=%d" (if (< 3 2) (+ 3 4) (+ 4 5))) "if=9"]
    [(printc "if=%d" (if 0 12)) "if=12"]
    [(printc "if=%d" (if (> 3 2) (+ 3 4) )  ) "if=7"]

    [(printc "if=%d" 
        (if (= 1 1)
            0
            (if (= 1 1)
                1
                2
                )
            ))
            "if=0"]
    [(printc "ret=%d" (let ((a 10) (b 11) (c 12) (d 13)) 1 2 3 4 (let () 5 6 )) ) "ret=6"]
    [(printc "ret=%d %d"  
        (let ((a 10) (b 11) (c 12) (d 13))
            (printc "call a=%d " a)
            (printc "call2 b=%d " b)
            (printc "call3 c=%d " c)
            (printc "call4 d=%d " d)
            1 2 3 4 
            (let () 5 6 )
            )
            (let ((a 20) (b 21) (c 22) )
            (printc "mid call a=%d " a)
            (printc "mid call2 b=%d " b)
            (printc "mid call3 c=%d " c)
            (printc "mid call4 d=%d " d)
            1 2 3 4 
            (let () 5 6 )
            )
            ) "call a=10 call2 b=11 call3 c=12 call4 d=13 mid call a=20 mid call2 b=21 mid call3 c=22 mid call4 d=13 ret=6 6"]
    [(printc "%s %d" "(+ 1 100000)=" (+ 1 100000)) "(+ 1 100000)= 100001"]

    [(printc "ret=%d" 
        (let loop2 ((i 0) )
            (printc "i -> %d" i)
            (loop2 (+ i 1))
            )) 
            '()]
  )

(test-all)
