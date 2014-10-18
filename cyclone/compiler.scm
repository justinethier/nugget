;;
;; Front-end for the compiler itself
;;
(load "bootstrap-chicken.scm")
;(load "bootstrap-husk.scm")
(load "parser.scm")
(load "trans.scm")

;; Code emission.
  
; c-compile-and-emit : (string -> A) exp -> void
(define (c-compile-and-emit input-program)
  (trace:info "---------------- input program:")
  (trace:info input-program) ;pretty-print
  
  (if *do-desugar*
    (begin
      (set! input-program (desugar input-program))
      (trace:info "---------------- after desugar:")
      (trace:info input-program))) ;pretty-print

  (if *do-cps*
    (begin
      (set! input-program (cps-convert input-program))
      (trace:info "---------------- after CPS:")
      (trace:info input-program))) ;pretty-print

  (analyze-mutable-variables input-program)

  (set! input-program (desugar (wrap-mutables input-program)))
  (trace:info "---------------- after wrap-mutables:")
  (trace:info input-program) ;pretty-print

  (set! input-program 
    (caddr ;; Strip off superfluous lambda
      (closure-convert input-program)))
  (trace:info "---------------- after closure-convert:")
  (trace:info input-program) ;pretty-print
  
  (if (not *do-code-gen*)
    (begin
      (trace:error "DEBUG, existing program")
      (exit)))

  (load "cgen.scm")
  (trace:info "---------------- C code:")
  (mta:code-gen input-program))


;; Compile and emit:

(let ((args (command-line-arguments)))
  (if (< (length args) 1)
      (error "usage: cyc file.scm"))

  (call-with-input-file (car (command-line-arguments))
    (lambda (port)
      (c-compile-and-emit 
        (cyc-read-all port)))))

;(define the-program
;    (cons 'begin (read-all))) ;; read-all is non-standard
;
;(c-compile-and-emit the-program)
;
