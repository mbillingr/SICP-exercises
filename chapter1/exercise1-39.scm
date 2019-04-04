#!/usr/bin/env -S guile -s
!#

(define (cont-frac n d k)
  (define (iterate i result)
    (if (= i 0)
        result
        (iterate (- i 1)
                 (/ (n i)
                    (+ result (d i))))))
  (iterate k 0))

(define (tan x k)
  (/ (cont-frac (lambda (i) (- (sqr x)))
                (lambda (i) (- 1 (* i 2)))
                k)
     x))

(display (tan 3 10))
(newline)
