#!/usr/bin/env -S guile -s
!#

(define (f g) (g 2))

(display (f sqr))
(newline)

(display (f (lambda (z) (* z (+ z 1)))))
(newline)

(display "calling (f f) should cause an error.")
(display "\n (f f)  =>  (f 2)  =>  (2 2)\n")
(f f)
