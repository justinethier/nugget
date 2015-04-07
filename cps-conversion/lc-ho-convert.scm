(require-extension matchable) ;; CHICKEN 4 egg

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
    [`(lambda . ,_)      (k (M expr))]
    [ (? symbol?)   (k (M expr))]
    [`(,f ,e)    
      ; =>
      (define $rv (gensym '$rv))
      (define cont `(lambda (,$rv) ,(k $rv)))
      (T f (lambda ($f)
             (T e (lambda ($e)
                    `(,$f ,$e ,cont)))))]))
     
(define (M expr)
  (match expr
    [`(lambda (,var) ,expr)
      ; =>
      (define $k (gensym '$k))
     `(lambda (,var ,$k) ,(T expr (lambda (rv) `(,$k ,rv))))]
    
    [(? symbol?)  #;=>  expr]))
     

;; Examples

(M '(lambda (x) x))

(T '(g a) (lambda (ans) `(halt ,ans)))

