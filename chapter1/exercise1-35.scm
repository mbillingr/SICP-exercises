#!/usr/bin/env -S guile -s
!#

(display "The golden ratio is defined as\n")
(display "    φ = a / b = (a + b / b) with a > b > 0.\n")
(display "If we set b = 1, we get\n")
(display "    a = (a + 1)/a = 1 + 1/a.\n")
(display "Thus, φ is the fixed point of the transformation\n")
(display "    a -> 1 + 1/a.\n")
(newline)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
(newline)
