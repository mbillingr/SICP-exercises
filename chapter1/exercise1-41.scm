#!/usr/bin/env -S guile -s
!#

(define (double f)
  (lambda (x) (f (f x))))

(display (((double (double double)) inc) 5))
(newline)
