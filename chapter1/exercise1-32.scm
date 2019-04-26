(import (builtin core)
        (sicp utils))

(define (accumulate combiner null-value transform a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (transform a)
                                 result))))
  (iter a null-value))

(define (accumulate-recursive combiner null-value transform a next b)
  (if (> a b)
      null-value
      (combiner (transform a)
                (accumulate-recursive combiner
                                      null-value
                                      transform
                                      (next a)
                                      next
                                      b))))


(define (product fact a next b)
  (accumulate-recursive * 1 fact a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc 10))

(define (cumulate n)
  (sum identity 1 inc 10))

(display (factorial 10))
(newline)
(display (cumulate 10))
(newline)
