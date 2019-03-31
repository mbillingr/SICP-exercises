#!/usr/bin/env -S guile -s
!#

(define (f-recursive n)
    (if (< n 3)
        n
        (+ (f-recursive (- n 1))
           (* 2 (f-recursive (- n 2)))
           (* 3 (f-recursive (- n 3))))))

(define (f-iterative a b c n)
    (if (= n 0)
        c
        (f-iterative (+ a (* 2 b) (* 3 c))
                     a b (- n 1))))

(define (f n) (f-iterative 2 1 0 n))

(display (f 22))
(newline)
(display (f-recursive 22))
