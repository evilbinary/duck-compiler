;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (logger)
  (export 
  log-level
  log-info
  log-debug
  log-error
  )
  (import (scheme))

  (define (log-level level s . args)
    (apply printf s args)
    (newline ))

   (define (log-info s  args)
    (log-level 'info s args)
    )

    (define (log-debug s  args)
    (log-level 'info s args)
    )

    (define (log-error s  args)
        (log-level 'info s args)
    )
)