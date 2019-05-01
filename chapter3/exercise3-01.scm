(import (builtin core)
        (sicp utils))

(define (make-accumulator acc)
  (lambda (amount)
    (set! acc (+ amount acc))
    acc))

(define A (make-accumulator 5))

(println (A 10))
(println (A 10))
