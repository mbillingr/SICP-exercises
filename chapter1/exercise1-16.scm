(import (builtin core)
        (sicp utils))

; iterative exponentiation in O(log n) time

(define (exp-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (exp-iter a
                             (* b b)
                             (/ n 2)))
        (else (exp-iter (* a b)
                        b
                        (- n 1)))))

(define (exp base exponent)
  (exp-iter 1 base exponent))

(display (exp 3 6))
(newline)
