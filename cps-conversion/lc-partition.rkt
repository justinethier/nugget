#lang racket

; Input language:

; <expr> ::= (λ (<var>) <expr>)
;         |  <var>
;         |  (<expr> <expr>)


; Output language:

; <uexp> ::= (λ (<uvar> <kvar>) <call>)
;         |  <uvar>
; <kexp> ::= (κ (<uvar>) <call>)
;         |  <kvar>

; <call> ::= ucall | kcall

; <ucall> ::=  (<uexp> <uexp> <kexp>)
; <kcall> ::=  (<kexp> <uexp>)


; Generate a continuation variable:
(define (genksym kv)
  (gensym (string->symbol (string-append "$$k" (symbol->string kv)))))

; Generate a user variable:
(define (genusym uv)
  (gensym (string->symbol (string-append "$$u" (symbol->string uv)))))
  
; Syntax for continuations:
(define-syntax κ
  (syntax-rules ()
    [(_ (uvar) call)   (λ (uvar) call)]))


; Transform with a meta-continuation:
(define (T-k expr k)
  (match expr
    [`(λ . ,_)      (k (M expr))]
    [ (? symbol?)   (k (M expr))]
    [`(,f ,e)    
      ; =>
      (define $rv (genusym '$rv))
      (define cont `(κ (,$rv) ,(k $rv)))
      (T-k f (λ ($f)
             (T-k e (λ ($e)
                    `(,$f ,$e ,cont)))))]))

; Transform with a syntactic continuation:
(define (T-c expr c)
  (match expr
    [`(λ . ,_)     `(,c ,(M expr))]
    [ (? symbol?)  `(,c ,(M expr))]
    [`(,f ,e)    
      ; =>
      (T-k f (λ ($f)
               (T-k e (λ ($e)
                        `(,$f ,$e ,c)))))]))
     
; Transform an atomic expression:
(define (M expr)
  (match expr
    [`(λ (,var) ,expr)
      ; =>
      (define $k (genksym '$k))
     `(λ (,var ,$k) ,(T-c expr $k))]
    
    [(? symbol?)  #;=>  expr]))
     



;; Examples

(M '(λ (x) x))

(T-c '(g a) 'halt)

(T-c '((f x) a) 'halt)


