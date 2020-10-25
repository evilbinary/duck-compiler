;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (duck libs)
  (export 
    print-dot
    print-list
    print-value

    emit-print-const-value
    emit-print-type-value
    emit-print-value
    emit-printf
    emit-alloc
    emit-free
    emit-prim
    prim?
    binop?
  )

(import
    (common common)
    (common match)
    (common trace)
    (duck primitive)
    )  
)