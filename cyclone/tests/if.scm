;(if #t 1 2)
(if (+ 1 2) (+ 3 4) (* 3 4))
(if ((lambda (x) (+ x 1)) 0) (+ 1 1) (* 0 0))

;; TODO: a few problems in compile code, but first priority is GC.
;; for some reason, after the second major collection, gc_cont is 
;; never transported, so the pointer becomes invalid. what is
;; going on there?
