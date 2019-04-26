(import (builtin core)
        (sicp utils))

(define (compose f g)
  (lambda (x) (f (g x))))

; recursive
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

; iterative
(define (repeated f n)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (- i 1) (compose f result))))
  (iter n f))

(display ((repeated sqr 2) 5))
(newline)
