#!/usr/bin/env -S guile -s
!#

(define (integrate f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((= k 0) (y k))
          ((= k n) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (sum term 0 inc n)
     (/ h 3)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(display (integrate cube 0 1 100))

;; causes stack overflow
;(display (integrate cube 0 1 1000))
