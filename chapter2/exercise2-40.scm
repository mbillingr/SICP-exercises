#!/usr/bin/env -S guile -s
!#

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (define (smallest-divisor) (find-divisor 2))
  (define (find-divisor test-divisor)
      (cond ((> (sqr test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor (next test-divisor)))))
  (define (next n) (if (= n 2) 3 (+ n 2)))
  (define (divides? a b) (= (remainder b a) 0))
  (= n (smallest-divisor)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(display (prime-sum-pairs 6))
(newline)
