#lang racket

; Input language:

; <expr> ::= (λ (<var>) <expr>)
;         |  <var>
;         |  (<expr> <expr>)



; Output language:

; <aexp> ::= (λ (<var>*) <cexp>)
;         |  <var>

; <cexp> ::=  (<aexp> <aexp>*)


(define (T expr cont)
  (match expr
    [`(λ . ,_)     `(,cont ,(M expr))]
    [ (? symbol?)  `(,cont ,(M expr))]
    [`(,f ,e)    
      ; =>
      (define $f (gensym '$f))
      (define $e (gensym '$e))
      (T f `(λ (,$f)
              ,(T e `(λ (,$e)
                       (,$f ,$e ,cont)))))]))
     
(define (M expr)
  (match expr
    [`(λ (,var) ,expr)
      ; =>
      (define $k (gensym '$k))
     `(λ (,var ,$k) ,(T expr $k))]
    
    [(? symbol?)  #;=>  expr]))
     

;; Examples:

(M '(λ (x) x))

(T '(g a) 'halt)

