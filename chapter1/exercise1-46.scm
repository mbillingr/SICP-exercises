#!/usr/bin/env -S guile -s
!#

(define tolerance 1e-12)

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (display guess) (newline)
    (let ((next (improve guess)))
         (if (good-enough? guess next)
             next
             (iter next))))
  iter)

(define (fixed-point f first-guess)
  (define (good-enough? guess next)
    (< (abs (- guess next)) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (fixed-point improve 1.0))

(display (sqrt 2))
(newline)
