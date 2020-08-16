;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (options)
  (export 
    option-set
    option-get
    make-options
  )

  (import (scheme))

  (define (make-options)
    (make-hashtable equal-hash equal?)
  )
  (define duck-options (make-options))

  (define (option-set var val)
    (hashtable-set! duck-options var val)
  )
  
  (define option-get
    (case-lambda 
      [(var val)
        (hashtable-ref duck-options var val)
      ]
      [(var)
        (option-get var '())
      ]))

)