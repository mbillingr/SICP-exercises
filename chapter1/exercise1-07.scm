#!/usr/bin/env -S guile -s
!#

(define (sqrt x) (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
    (define improved-guess (improve guess x))
    (if (good-enough? guess improved-guess) 
        improved-guess
        (sqrt-iter improved-guess x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average a b)
    (/ (+ a b)
       2))

(define (good-enough? guess new-guess)
    (< (abs (/ (- guess new-guess) guess))
       1e-12))

(define (sqr x)
    (* x x))

(display (sqr (sqrt 9e-9))) (newline)
(display (sqr (sqrt 9))) (newline)
(display (sqr (sqrt 9e9))) (newline)
