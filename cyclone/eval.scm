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
        ((boolean? exp) #t)
        ;((string? exp) true)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) ;; TODO: add (not) support
      (cadddr exp)
      #f))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
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
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))
         ;(lambda () 'TODO-unknown-exp-type)))) ; JAE - this is a debug line

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

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (operands exp))) ; TODO: (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
; TODO:                           (map (lambda (aproc) (aproc env))
        aprocs)))) ;; TODO: temporary testing w/constants
; TODO:                                aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply proc args))
         ;(apply-primitive-procedure proc args))
;; TODO:
;        ;((compound-procedure? proc)
;        ; ((procedure-body proc)
;        ;  (extend-environment (procedure-parameters proc)
;        ;                      args
;        ;                      (procedure-environment proc))))
        (else
#f))) ;; TODO: this is a temporary debug line
;         (error
;          "Unknown procedure type -- EXECUTE-APPLICATION"
;          proc))))

TODO: operations on environments

;; TODO: temporary testing
;; also, it would be nice to pass around something other than
;; symbols for primitives. could the runtime inject something into the env?
;; of course that is a problem for stuff like make_cons, that is just a
;; C macro...
(define (primitive-procedure? proc)
  (equal? proc 'cons))


;; JAE - Testing, should work both with cyclone and other compilers (husk, chicken, etc)
;;       although, that may not be possible with (app) and possibly other forms. 
(define *global-environment* '())
;(write (eval 2 *global-environment*))
;(write (eval ''(1 2) *global-environment*))
;(write (eval ''(1 . 2) *global-environment*))
;(write (eval '(if #t 'test-ok 'test-fail) *global-environment*))
;(write (eval '(if 1 'test-ok) *global-environment*))
;(write (eval '(if #f 'test-fail 'test-ok) *global-environment*))
(write (eval '(cons 1 2) *global-environment*)) ; TODO
;(write (eval '(+ 1 2) *global-environment*)) ; TODO

