;; The meta-circular evaluator from SICP 4.1
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1
;;

(define (eval exp env)
  ((analyze exp) env))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (equal? (car exp) tag)
      #f))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ;((string? exp) true)
        (else #f)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) ;; TODO: null? and cdddr not in cyclone
      (cadddr exp)
      #f))

(define (application? exp) (pair? exp))
;(define (operator exp) (car exp))
;(define (operands exp) (cdr exp))
;(define (no-operands? ops) (null? ops))
;(define (first-operand ops) (car ops))
;(define (rest-operands ops) (cdr ops))

; TODO: add supported primitives. also, this may not be the
; best place for this??
(define (primitive-procedure? proc)
 #f)

; TODO: 
;(define (apply-primitive-procedure proc args)
;  (apply-in-underlying-scheme
;   (primitive-implementation proc) args))

;; Improvement from section 4.1.7 - Separate syntactic analysis from execution
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
         ((quoted? exp) (analyze-quoted exp))
        ;((variable? exp) (analyze-variable exp))
        ;((assignment? exp) (analyze-assignment exp))
        ;((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ;((lambda? exp) (analyze-lambda exp))
       ; TODO: ideally, macro system would handle these two
       ;((begin? exp) (analyze-sequence (begin-actions exp)))
       ;((cond? exp) (analyze (cond->if exp)))
        ;((application? exp) (analyze-application exp))
        (else
        ; (error "Unknown expression type -- ANALYZE" exp))))
         (lambda () 'TODO-unknown-exp-type)))) ; JAE - this is a debug line

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (cadr exp)))
    (lambda (env) qval)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (pproc env)
          (cproc env)
          (aproc env)))))

;(define (analyze-application exp)
;  (let ((fproc (analyze (operator exp)))
;        (aprocs (map analyze (operands exp))))
;    (lambda (env)
;      (execute-application (fproc env)
;                           (map (lambda (aproc) (aproc env))
;                                aprocs)))))
;(define (execute-application proc args)
;  (cond ((primitive-procedure? proc)
;         (apply-primitive-procedure proc args))
;; TODO:
;        ;((compound-procedure? proc)
;        ; ((procedure-body proc)
;        ;  (extend-environment (procedure-parameters proc)
;        ;                      args
;        ;                      (procedure-environment proc))))
;        (else
;         (error
;          "Unknown procedure type -- EXECUTE-APPLICATION"
;          proc))))

;; JAE - Testing
(write (eval 2 #f))
(write (eval ''(1 2) #f))
(write (eval ''(1 . 2) #f))
