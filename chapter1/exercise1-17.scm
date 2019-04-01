#!/usr/bin/env -S guile -s
!#

(define (mul x y)
  (cond ((= x 0) 0)
        ((= y 0) 0)
        ((even? y) (mul (double x)) (halve y))
        (else (+ x (mul x (- y 1))))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(display (mul 3 6))
