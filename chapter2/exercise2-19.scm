(import (builtin core)
        (sicp utils))

(define us-coins (list 50 25 10 5 1))
(define su-coins (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 5 2 1 0.5))
(define eu-coins (list 200 100 50 20 10 5 2 1))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(display (cc 100 us-coins)) (newline)
(display (cc 100 su-coins)) (newline)
(display (cc 50 uk-coins)) (newline)
(display (cc 100 eu-coins)) (newline)

; The result seems to be unaffected by the order of coin values.
; This is the expected correct behavior of the program - we want
; to find all combinations, and their number does not depend on
; the order.
