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

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((miller-rabin-test n) (fast-prime? n (- times 1)))
          (else false)))

(define true #t)
(define false #f)

(define (miller-rabin-test n)
    (define (try-it a)
        (= (expmod a (- n 1) n) 1))  ; changing this line seems to be sufficient
    (try-it (+ 1 (random (- n 1)))))

; The book suggests that the expmod procedure should be changed to
; detect "nontrivial square roots of 1 modulo n". However, it seems
; changing the line indicated above was all that's needed...
(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) (remainder
                           (square (expmod base (/ exp 2) m))
                           m))
          (else (remainder 
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
    (display (fast-prime? n 1000))
    (newline))


(display "the miller-rabin test works for all numbers:\n")
(compare 1999)
(compare 19999)
(display "even for those where the fermat test failed:\n")
(compare 561)
(compare 6601)
