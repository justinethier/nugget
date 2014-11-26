;; TODO: relocate this stuff into a common place, and delete this file
(define a cons)
(write (a 1 2))
;(define (foldr func end lst)
;  (if (null? lst)
;      end
;          (func (car lst) (foldr func end (cdr lst)))))
;
;(define (map func lst)        (foldr (lambda (x y) (cons (func x) y)) '() lst))
;
;(write (map car '((a . b) (1 . 2) (#\h #\w))))
