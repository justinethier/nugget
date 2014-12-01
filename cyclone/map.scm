;; TODO: relocate this stuff into a common place, and delete this file

(define a length)
(write a)

;; TODO: this is broken when using cons by itself. 
;; we do not handle primitives as first-class functions
(define a cons)
;(define a (lambda (x y) (cons x y)))
(write (a 1 2))

;;(define (foldr func end lst)
;;  (if (null? lst)
;;      end
;;          (func (car lst) (foldr func end (cdr lst)))))
;;
;;(define (map func lst)        (foldr (lambda (x y) (cons (func x) y)) '() lst))
;;
;;; TODO: same problem as above; works fine if primitive is wrapped:
;;(write (map (lambda (x) (car x)) '((a . b) (1 . 2) (#\h #\w))))
;;;(write (map car '((a . b) (1 . 2) (#\h #\w))))
