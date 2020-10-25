;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (arch arch)
  (export 
    ;;regs
    reg0 reg1 reg2 reg3 reg4 reg5 reg6 reg7 regs regs-map
    ;;instruct
    asm set mref mset note
    add label sar sal mul sub div
    shl shr ret
    call jmp cmp-jmp cmp
    land lor xor save restore
    nop local proc pret lproc lret
    fcall ccall
    stext sexit 
    data sdata
    asm-compile-exp
    arch-bits
  )

(import
    (common common)
    (common match)
    (common trace)
    (duck options)
    (rename (scheme) (div div2) )
    ; (x86)
    ; (wasm)
    )

(define-syntax import-arch
  (lambda (x)
      (syntax-case x ()
        ((_ k)
          (datum->syntax #'k `(import 
          ,(case (option-get 'arch 'x86)
            ['x86 '(arch x86)]
            ['wasm '(arch wasm)]
            ['llvm '(arch llvm)]
            [else (error 'platform "not support ")]
            ))
          ))))
  )
  
  (import-arch options)
  
)