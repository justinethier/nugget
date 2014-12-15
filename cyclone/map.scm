;; TODO: relocate this stuff into a common place, and delete this file

(define a length)
(write a)

;; TODO: this is broken when using cons by itself. 
;; we do not handle primitives as first-class functions
(define a cons)
;(define a (lambda (x y) (cons x y)))
(write (a 1 2))

;; Testing internal defines
;; The aa should be turned into a (set!)
(define (test)
  (define aa 1)
  aa)
(define (test2)
; TODO: need to be able to shadow globals, too
;       using an internal define
 (define a 1)
 a)
;; lambda var able to shadow global
(define (test3)
  ((lambda (a) a) 1))

(define (foldr func end lst)
  (if (null? lst)
      end
          (func (car lst) (foldr func end (cdr lst)))))

(define (map func lst)        (foldr (lambda (x y) (cons (func x) y)) '() lst))

(write (map (lambda (x) (car x)) '((a . b) (1 . 2) (#\h #\w))))
(write (map car '((a . b) (1 . 2) (#\h #\w))))
(write (map cdr '((a . b) (1 . 2) (#\h #\w))))
(write (map length '((1) (1 2) (1 2 3) (1 2 3 4))))

; TODO: looks like a parse problem with () below:
;(write (map length '(() (1) (1 2) (1 2 3) (1 2 3 4))))
