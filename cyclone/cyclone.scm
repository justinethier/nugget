;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module contains a front-end for the compiler itself.
;;

(load "bootstrap-chicken.scm")
;(load "bootstrap-husk.scm")
(load "parser.scm")
(load "trans.scm")
(load "cgen.scm")

(define *version-banner* "
              :@ 
            @@@  
          @@@@:  
        `@@@@@+  
       .@@@+@@@      Cyclone  
       @@     @@     An experimental Scheme compiler
      ,@             TODO: project URL
      '@        
      .@        
       @@     #@     (c) 2014 Justin Ethier
       `@@@#@@@.     Version 0.0.1 (Pre-release) 
        #@@@@@   
        +@@@+    
        @@#      
      `@.  
     
")

(define *c-file-header-comment* "/**
 ** This file was automatically generated by the Cyclone scheme compiler
 **
 ** (c) 2014 Justin Ethier
 ** Version 0.0.1 (Pre-release)
 **
 **/
")

;; Code emission.
  
; c-compile-and-emit : (string -> A) exp -> void
(define (c-compile-and-emit input-program)
  (emit *c-file-header-comment*) ; Guarantee placement at top of C file

  (trace:info "---------------- input program:")
  (trace:info input-program) ;pretty-print
  
  (set! input-program (expand input-program))
  (trace:info "---------------- after macro expansion:")
  (trace:info input-program) ;pretty-print

  (set! input-program 
    (alpha-convert
        input-program))
  (trace:info "---------------- after alpha conversion:")
  (trace:info input-program) ;pretty-print

  (set! input-program (cps-convert input-program))
  (trace:info "---------------- after CPS:")
  (trace:info input-program) ;pretty-print

  (analyze-mutable-variables input-program)

  (set! input-program (wrap-mutables input-program))
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

  (trace:info "---------------- C code:")
  (mta:code-gen input-program))

;; Compile and emit:
(define (run-compiler args cc?)
  (let* ((in-file (car args))
         (exec-file (basename in-file))
         (src-file (string-append exec-file ".c")))
    (call-with-input-file in-file
      (lambda (port)
        (let ((program (cyc-read-all port)))
;; TODO: husk does not support with-output-to-file
;; will be a problem bootstrapping from husk in the meantime...
          (with-output-to-file 
            src-file 
            (lambda ()
              (c-compile-and-emit 
                (cons 'begin program))))
          (if cc?
            (system 
              ;; -I is a hack, real answer is to use 'make install' to place .h file
              (string-append "gcc " src-file " -I. -g -o " exec-file))))))))

;; Handle command line arguments
(let ((args (command-line-arguments))) ;; TODO: port (command-line-arguments) to husk??
  (cond
    ((< (length args) 1)
     (display "cyclone: no input file")
     (newline))
    ((member "-h" args)
     (display "TODO: display help text")
     (newline))
    ((member "-v" args)
     (display *version-banner*))
    ((member "-d" args)
     (run-compiler args #f)) ;; Debug, do not run GCC
    (else
      (run-compiler args #t))))

