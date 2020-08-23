;; test
(import (scheme) 
  (test)  
)

(add-test-string "test asm"
  ; [($asm (block all
  ;           (block main 
  ;             (set reg0 reg1)
  ;             (set reg0 reg1)
  ;             )))   ""]
  ; [($asm (set reg0 reg1)) ""]

  [(begin 
    (define (print-ch ch)
    ($asm
        (call print-char #x4f62))
    )
    (print-ch #x4f62)

    ($asm
        (label halt)
        (jmp halt)

        ;;打印一个字符
        (proc print-char)
        (set reg0 (local 0))
        (set reg5 #xb8000)
        (mset reg5 r0)
        (ret)
  ))

  "" 
  ]
  
  )

(test-all)
