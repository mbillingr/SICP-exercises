#!/usr/bin/env -S guile -s
!#

(define (cube-root x) (curt-iter 1.0 x))

(define (curt-iter guess x)
    (define improved-guess (improve guess x))
    (if (good-enough? guess improved-guess)
        improved-guess
        (curt-iter improved-guess x)))

(define (improve y x)
    (/ (+ (/ x (sqr y))
          (* 2 y))
       3))

(define (average a b)
    (/ (+ a b)
       2))

(define (good-enough? guess new-guess)
    (< (abs (/ (- guess new-guess)
               guess))
       1e-12))

(define (sqr x) (* x x))
(define (cube x) (* x x x))

(display (cube (cube-root 9e-9))) (newline)
(display (cube (cube-root 9))) (newline)
(display (cube (cube-root 9e9))) (newline)

