#!/usr/bin/env -S guile -s
!#

(define (make-segment a b)
  (cons a b))
(define start-segment car)
(define end-segment cdr)

(define (make-point x y)
  (cons x y))
(define x-point car)
(define y-point cdr)

(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

(define s (make-segment (make-point 1 2) (make-point 5 3)))

(display (midpoint-segment s))
(newline)
