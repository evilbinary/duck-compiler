;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (duck type)
  (export 
  type-shift
  type-mask
  fixnum-tag
  pair-tag
  box-tag
  vector-tag
  closure-tag
  string-tag
  symbol-tag
  const-tag
  const-mask
  boolean-tag
  boolean-mask

  true-tag
  false-tag
  null-tag
  true-rep
  false-rep
  null-rep
  void-rep
  void-tag
  type-rep

  type?
  type-value
  )

(import (scheme)
    (common match)
    (common trace)
    (common common)
    )
  
;;;types
(define type-shift 3)
(define type-mask #b111)
;;types tag
(define fixnum-tag   #b000)
(define pair-tag     #b001)
(define box-tag      #b010)
(define vector-tag   #b011)
(define closure-tag  #b100)
(define string-tag  #b110)
(define symbol-tag  #b111)

;;const
(define const-tag #b101)
(define const-mask #b111111)

(define boolean-tag    #b1101)
(define boolean-mask   #b1111)

(define true-tag     #b111)
(define false-tag    #b101)

(define null-tag     #b100)
(define void-tag     #b110)

(define true-rep     #b111101)
(define false-rep    #b101101)

(define null-rep     #b100101)
(define void-rep     #b110101)


(define (type-rep x)
    (cond
      [(equal? (void) x) void-rep]
      [(integer? x) 
        (bitwise-arithmetic-shift-left x type-shift)]
      [(boolean? x) (if x true-rep false-rep)]
      [(or (null? x)
           (and (pair? x) (equal? (car x) 'quote ))
           (and (pair? x) (null? (cadr x))) )
        null-rep]
      [(symbol? x)
        x
      ]
      [(string? x)
        x
      ]
      [else 
      (error 'type-rep "unsupport primitive " x)
      ]
      ))

(define (type-pair? x)
 (cond 
   [(integer? x) (= (logand x type-mask) pair-tag) ]
   [else #f]
  )
)

(define (type-fixnum? x)
 (cond 
   [(integer? x) (= (logand x type-mask) fixnum-tag)]
   [else #f]
  )
)

(define (type-string? x)
 (cond 
   [(integer? x) (= (logand x type-mask) string-tag)]
   [else #f]
  )
)

(define (type-vector? x)
  (cond 
   [(integer? x) (= (logand x type-mask) vector-tag) ]
    [else #f]
  )
)

(define (type-box? x)
  (cond 
   [(integer? x) (= (logand x type-mask) box-tag) ]
   [else #f]
  )
)

(define (type-const? x)
  (cond 
     [(integer? x) (or (type-boolean? x) (type-null? x) (type-void? x)) ]
     [else #f]
  )
)

(define (type-boolean? x)
  (cond 
    [(symbol? x)
     (or (equal? x 'true-rep) (equal? x 'false-rep))]
    [(integer? x) (= (logand x boolean-mask) boolean-tag) ]
    [else #f]
  )
)

(define (type-null? x)
  (cond 
    [(symbol? x) (equal? x 'null-rep)]
    [(integer? x) (= x null-rep) ]
    [else #f]
  )
)

(define (type-void? x)
  (cond 
    [(symbol? x) (equal? x 'void-rep)]
    [(integer? x) (= x void-rep) ]
    [else #f]
  )
)

(define (type-symbol? x)
  (cond 
    [(symbol? x) (not (or (type-void? x) 
                       (type-boolean? x) 
                       (type-fixnum? x)
                       (type-null? x) ))
                       ]
    [else #f]
  )
)

(define (type-value x)
  (cond
    [(type-boolean? x) x]
    [(type-void? x) void-rep]
    [(type-null? x) null-rep]
    [(type-const? x) x]
    [(type-fixnum? x) (bitwise-arithmetic-shift-right x type-shift) ]
    [(type-pair? x) (bitwise-arithmetic-shift-right x type-shift) ] 
    [(type-vector? x) (bitwise-arithmetic-shift-right x type-shift)]
    [(type-string? x) (bitwise-arithmetic-shift-right x type-shift)]
    [(type-box? x) (bitwise-arithmetic-shift-right x type-shift)]
    [(type-symbol? x) x]
    [else (error 'type-value "unsupport primitive" x)]
  )
)

(define (type? x)
  (cond
      [(pair? x) #f]
      [(type-void? x) #t]
      [(type-null? x) #t]
      [(type-boolean? x) #t]
      [(type-const? x) #t]
      [(type-fixnum? x) #t]
      [(type-symbol? x) #t]
      [else #f]
    )
)

)