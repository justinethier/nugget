; A simple use of macros
; TODO: this is broken when begin is left in... WTF?
(begin
  (let ((x 1)
        (y 2)
        (z 4))
    (display (+ x (- z y)))
    (+ y z))
)
