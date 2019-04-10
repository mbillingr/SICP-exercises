#!/usr/bin/env -S guile -s
!#

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coef higher-terms) (+ this-coef
                                                  (* x higher-terms)))
              0
              coefficient-sequence))


(display (horner-eval 2 (list 1 3 0 5 0 1)))
(newline)
