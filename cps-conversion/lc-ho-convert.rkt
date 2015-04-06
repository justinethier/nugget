#lang racket

; Input language:

; <expr> ::= (λ (<var>) <expr>)
;         |  <var>
;         |  (<expr> <expr>)


; Output language:

; <aexp> ::= (λ (<var>*) <cexp>)
;         |  <var>

; <cexp> ::=  (<aexp> <aexp>*)


(define (T expr k)
  (match expr
    [`(λ . ,_)      (k (M expr))]
    [ (? symbol?)   (k (M expr))]
    [`(,f ,e)    
      ; =>
      (define $rv (gensym '$rv))
      (define cont `(λ (,$rv) ,(k $rv)))
      (T f (λ ($f)
             (T e (λ ($e)
                    `(,$f ,$e ,cont)))))]))
     
(define (M expr)
  (match expr
    [`(λ (,var) ,expr)
      ; =>
      (define $k (gensym '$k))
     `(λ (,var ,$k) ,(T expr (λ (rv) `(,$k ,rv))))]
    
    [(? symbol?)  #;=>  expr]))
     

;; Examples

(M '(λ (x) x))

(T '(g a) (λ (ans) `(halt ,ans)))

