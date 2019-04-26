(import (builtin core)
        (sicp utils))

(define (cont-frac-recursive n d k)
  (define (recurse i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (recurse (+ i 1))))))
  (recurse 1))

(define (cont-frac n d k)
  (define (iterate i result)
    (if (= i 0)
        result
        (iterate (- i 1)
                 (/ (n i)
                    (+ result (d i))))))
  (iterate k 0))

(display "expected: 0.6180\n")

(display (cont-frac-recursive (lambda (i) 1.0)
                              (lambda (i) 1.0)
                              12))
(newline)

(display (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    12))
(newline)
