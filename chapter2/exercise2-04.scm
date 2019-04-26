(import (builtin core)
        (sicp utils))

; Here is an alternative procedural representation of pairs:

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; What is the corresponding definition of cdr?

(define (car z)
  (z (lambda (p q) q)))


;; Verify that (car (cons x y)) yields x for any objects x and y

; (car (cons x y)) = ((cons x y) (lambda (p q) p))
;                  = ((lambda (m) (m x y)) (lambda (p q) p))
;                  = ((lambda (p q) p) x y)
;                  = x
