;; Cyclone Scheme
;; Copyright (c) 2014, Justin Ethier
;; All rights reserved.
;;
;; This module contains a simple Read-Eval-Print Loop
;;
(display *Cyc-version-banner*)
(define (repl)
  (display "cyclone> ")
  (display (eval (read)))
  (display #\newline)
  (repl))
(repl)
