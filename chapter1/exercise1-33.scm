#!/usr/bin/env -S guile -s
!#

(define (filtered-accumulate predicate combiner null-value transform a next b)
  (define (filtered-combine a result)
    (if (predicate a)
        (combiner (transform a) result)
        result))
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (filtered-combine a result))))
  (iter a null-value))

(define (identity x) x)


;; todo: the interpreter drops the environment of prime-sos when tail-calling
;;       into filtered-accumulate and we can no longer look up next.

(define (part-b n)
  (define (relative-prime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate relative-prime? * 1 identity 1 inc n))

(define (prime-sos a b)
  (define (next n)
    (cond ((< n 2) 2)
          ((= n 2) 3)
          (else (+ n 2))))
  (filtered-accumulate prime? + 0 sqr a next b))

(define (prime? n)
  (and (not (= n 1))
       (= n (smallest-divisor n))))

(define (smallest-divisor n)
  (define (next n)
      (if (= n 2) 3 (+ n 2)))
  (define (find-divisor n test-divisor)
      (cond ((> (sqr test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (divides? a b) (= (remainder b a) 0))

(display "b) product of numbers relatively prime to n: ")
(display (part-b 5))
(newline)

(display "a) sum of squares of prime numbers: ")
(display (prime-sos 1 10))
(newline)
