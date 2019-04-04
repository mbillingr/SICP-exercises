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

(define (euler-n i) 1.0)

(define (euler-d i)
  (if (= (remainder i 3) 2)
      (* (+ i 1) (/ 2 3))
      1.0))

(define e (+ 2 (cont-frac euler-n euler-d 100)))

(display e)
(newline)
