#lang racket

; Input language:

; <expr> ::= (λ (<var>) <expr>)
;         |  <var>
;         |  (<expr> <expr>)


; Output language:

; <aexp> ::= (λ (<var>*) <cexp>)
;         |  <var>

; <cexp> ::=  (<aexp> <aexp>*)


(define (T-k expr k)
  (match expr
    [`(λ . ,_)      (k (M expr))]
    [ (? symbol?)   (k (M expr))]
    [`(,f ,e)    
      ; =>
      (define $rv (gensym '$rv))
      (define cont `(λ (,$rv) ,(k $rv)))
      (T-k f (λ ($f)
             (T-k e (λ ($e)
                      `(,$f ,$e ,cont)))))]))

(define (T-c expr c)
  (match expr
    [`(λ . ,_)     `(,c ,(M expr))]
    [ (? symbol?)  `(,c ,(M expr))]
    [`(,f ,e)    
      ; =>
      (T-k f (λ ($f)
               (T-k e (λ ($e)
                        `(,$f ,$e ,c)))))]))
     
(define (M expr)
  (match expr
    [`(λ (,var) ,expr)
      ; =>
      (define $k (gensym '$k))
     `(λ (,var ,$k) ,(T-c expr $k))]
    
    [(? symbol?)  #;=>  expr]))
     


;; Examples

(M '(λ (x) x))

(T-c '(g a) 'halt)

