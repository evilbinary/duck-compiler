;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(library (common)
  (export 
  string-replace
  type-case
  id
  void?
  try
  symbol->asm-id
  gen-sym
  symbol-append
  get-sym
  clear-sym
  asm-data-add
  asm-data-get
  asm-data-clear
  get-asm-data-define
  mapc
  asm
  note
  
   )

  (import 
    (scheme)
    (rename (scheme) (div div2) )
  )

(define mapc for-each)

(define (asm . args)
  (apply printf  args)
  (newline ))

(define note
  (case-lambda 
    [(a) (asm ";;~a" a)]
    [(fmt . val) 
      (printf ";;")
      (apply asm fmt val)]))

(define asm-data-define (make-hashtable equal-hash equal?))

(define (asm-data-add key val)
  (hashtable-set! asm-data-define (symbol->asm-id key) val)
)

(define (asm-data-get key val)
  (hashtable-ref asm-data-define key val)
)

(define (asm-data-clear)
  (set! asm-data-define (make-hashtable equal-hash equal?))
)

(define (get-asm-data-define)
  asm-data-define
)

(define symbols (make-hashtable equal-hash equal?))

(define (get-sym)
  symbols
)
(define (clear-sym)
  (set! symbols (make-hashtable equal-hash equal?))
)

(define (gen-sym prefix)
  (if (null? prefix )
    '()
    (let ((val (hashtable-ref symbols prefix 0)))
        (hashtable-set! symbols prefix (+ val 1))
        (string->symbol (string-append (symbol->string prefix) "." (number->string val)))
    )))

(define (symbol-append s1 s2)
   (string->symbol (string-append (symbol->string s1) "." (symbol->string s2)))
)

(define (symbol->asm-id s)
  (if (symbol? s)
      (set! s (symbol->string s )))
  (let ((str s))
    (set! str (string-replace "-" "." str))
    (set! str (string-replace " " ".space" str))
    (set! str (string-replace #\- #\. str))
    (set! str (string-replace #\space ".space" str))
    (set! str (string-replace #\+ ".add" str))
    (set! str (string-replace #\- ".minus" str))
    (set! str (string-replace #\* ".multi" str))
    (set! str (string-replace #\/ ".div" str))
    (set! str (string-replace #\= ".eq" str))
    (set! str (string-replace #\= ".eq" str))
    (set! str (string-replace #\% ".percent" str))
    (set! str (string-replace #\% ".percent" str))
    (set! str (string-replace #\( ".left_quote" str))
    (set! str (string-replace #\) ".right_quote" str))
    (set! str (string-replace #\[ ".sl_brackets" str))
    (set! str (string-replace #\] ".sr_brackets" str))
    (set! str (string-replace #\> ".larger" str))
    (set! str (string-replace #\\ #\_ str))
    (set! str (string-replace #\, ".comma"  str))
    (set! str (string-replace #\# ".b" str))
    (set! str (string-replace #\! ".ex" str))
    (set! str (string-replace #\newline ".newline" str))
    ;(printf ";symbol->asm-id:~a==>~a\n" s str)
    ; (let ((r (symbol->asm-id str)))
    ;    (if (not (string=? str r))
    ;     (set! str r)))
    str
    ; (format "str.~a" (string-hash str))
  )
)

(define (string-replace old new str)
  (let ((ss str))
    (cond 
      [(symbol? str)
          (set! ss (symbol->string str) )
          (let ((s (string-copy ss)))
            (string->symbol (string-replace-one old new s))
          )
        ]
      [(string? str)
        (let ((s (string-copy ss)))
          (string-replace-one old new s)
          )
      ]
      [else (error 'string-replace "unsupport type" str) ]
    )
  )
)

(define (string-index-of p ch)
  (let loop  ((len (string-length p))
                (i 0))
      (if (< i len)
        (begin 
          ;;(printf " i=~a=~a ~a\n" i (string-ref p i) ch )
          (if (char=? (string-ref p i) ch)
            i
            (loop len (+ i 1))
          ))
        -1
    ))
)

(define (string-replace-one old new str)
  (if (char? old)
    (set! old (string old))
  )
  (if (char? new)
    (set! new (string new))
  )
  (let ((len (string-length str))
        (len-old (string-length old))
        (len-new (string-length new))
        )
    (cond 
      [(= len 1) new]
      [(= len-old 1)  
        (let ((pos (string-index-of str (string-ref old 0))))
          ;;(printf "pos=>~a\n" pos)
          (if (>= pos 0)
           (let ((r (string-append (substring str 0 pos ) new (substring str (+ pos len-old) len )) ))
            ; (printf ";1=pos=~a old=~a ~a === ~a\n" pos old str r)
            r)
            str
          )
        )
      ]
      [else 
        (let ((pos (bmh str old)))
        ; (printf ";pos=>>~a\n" pos)
        (if (>= pos 0) 
          (let ((r (string-append (substring str 0 pos) new (substring str (+ pos len-old) len )) ))
            ; (printf ";;replace ~a === ~a\n" str r)
            r
          )
          str
          ))
      ]
    )
  ))

(define (compute-last-occ p)
  (let ((last-occ (make-vector 128 -1)))
    (let loop  ((len (- (string-length p) 1))
                (i 0))
      (if (< i len)
        (begin
          (vector-set! last-occ (char->integer (string-ref p i)) i)
          (loop len (+ i 1))
        )))
    last-occ
  ))

(define (bmh t p)
  (let ((last-occ (compute-last-occ p))
        (i0 0)
        (n (string-length t))
        (m (string-length p))
        (j -1)
        (ret -1)
        (is-not-ret #t) )
    (let loop ()
      (if (< i0 (- n m))
        (begin
          (set! j (- m 1))
          (let loop2 ()
            (if (char=? (string-ref p j) (string-ref t (+ i0 j)) )
              (begin 
                  (set! j (- j 1))
                  (if (< j 0)
                    (begin 
                      (set! is-not-ret #f)
                      (set! ret i0) )
                    (loop2)
                  ))))
          (if is-not-ret
            (begin
            (set! i0 (+ i0
                      (- m 1)
                      (-  (vector-ref last-occ 
                            (char->integer (string-ref t (+ i0 m -1))) )  )  ))
            (loop)))
        )))
    ret))

(define (id x) x)

(define (void? x)
  (equal? x (void))
)

(define-syntax type-case
  (syntax-rules (else)
    [(_ expr
        [(pred1 pred2 ...) e1 e2 ...] ...
        [else ee1 ee2 ...])
     (let ([t expr])
       (cond
         [(or (pred1 t) (pred2 t) ...) e1 e2 ...]
         ...
         [else ee1 ee2 ...]))]))


(define-syntax try 
  (syntax-rules (catch) 
    ((_ body (catch catcher)) 
     (call-with-current-continuation 
      (lambda (exit) 
	(with-exception-handler 
	 (lambda (condition) 
	   (catcher condition) 
	   (exit condition)) 
	 (lambda () body)))))))

)