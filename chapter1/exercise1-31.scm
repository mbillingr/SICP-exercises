#!/usr/bin/env -S guile -s
!#

(define (product fact a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result
                          (fact a)))))
  (iter a 1))

(define (product-recursive fact a next b)
  (if (> a b)
      1
      (* (fact a)
         (product-recursive fact (next a) next b))))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc 10))

(define (approx-pi n)
  (define (fact k)
    (if (even? k)
        (/ k (+ k 1))
        (/ (+ k 1) k)))
  (* 4 (product fact 2 inc (+ n 1))))

(display (factorial 10))
(newline)
(display (product-recursive identity 1 inc 10))
(newline)

(display (approx-pi 10))
(newline)
(display (approx-pi 1000))
(newline)
(display (approx-pi 100000))
(newline)
