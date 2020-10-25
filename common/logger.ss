;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (common logger)
  (export 
  log-level
  log-info
  log-debug
  log-error
  log-set-level
  )
  (import (scheme))
  (define level 'info)
  (define (log-set-level l)
    (set! level l)
  )
  (define (log-level l s . args)
    (if (not (null? level))
      (begin 
        (apply printf (format "[~a]~a" l s) args)
        (newline ))))

   (define (log-info . args)
    (apply log-level 'info args)
    )

    (define (log-debug . args)
      (apply log-level 'debug args)
    )

    (define (log-error . args)
       (apply log-level 'error args)
    )
)