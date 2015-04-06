
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
    [`(lambda . ,_)     `(,cont ,(M expr))]
    [ (? symbol?)  `(,cont ,(M expr))]
    [`(,f ,e)    
      ; =>
      (define $f (gensym '$f))
      (define $e (gensym '$e))
      (T f `(lambda (,$f)
              ,(T e `(lambda (,$e)
                       (,$f ,$e ,cont)))))]))
     
(define (M expr)
  (match expr
    [`(lambda (,var) ,expr)
      ; =>
      (define $k (gensym '$k))
     `(lambda (,var ,$k) ,(T expr $k))]
    
    [(? symbol?)  #;=>  expr]))
     

;; Examples:

(M '(lambda (x) x))

(T '(g a) 'halt)

