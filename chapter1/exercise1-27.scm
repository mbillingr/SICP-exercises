#!/usr/bin/env -S guile -s
!#

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

(define (next n)
    (if (= n 2) 3 (+ n 2)))

(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n) (= n (smallest-divisor n)))

(define true #t)
(define false #f)

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) 
            (remainder 
                (square (expmod base (/ exp 2) m))
                m))
          (else 
            (remainder 
                (* base (expmod base (- exp 1) m)) 
                m))))
          
(define (check-congruence n)
    (check-iter n 0))

(define (check-iter n a)
    (cond ((> a n) true)
          ((= (expmod a n n)
              (remainder a n))
            (check-iter n (+ a 1)))
          (else false)))

(define (compare n)
    (display n)
    (display ": ")
    (display (prime? n))
    (display " ")
    (display (check-congruence n))
    (newline))


(display "the fermat test usually works:\n")
(compare 1999)
(compare 19999)
(display "but is fooled by certain non-prime numbers:\n")
(compare 561)
(compare 6601)