(import (builtin core)
        (sicp utils))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


; one defined as add 1 to zero
(define one (lambda (f) ((add-1 zero) f)))

; substitute add-1
(define one  (lambda (f) (lambda (x) (f ((zero f) x)))))

; (zero f) is the identity function, so ((zero f) x) is x
(define one (lambda (f) (lambda (x) (f x))))

; so zero is the identity - f is never applied
; one is a function that applies f once
; then two should be f applied twice

; one defined as add 1 to zero
(define two (lambda (f) ((add-1 one) f)))

; substitute add-1
(define two  (lambda (f) (lambda (x) (f ((one f) x)))))

; ((one f) x) is f applied once to x (f x)
(define two  (lambda (f) (lambda (x) (f (f x)))))

; realize the church numeral by applying it with inc as f and 0 as x
(define (realize numeral)
  ((numeral inc) 0))

(display (realize zero))
(newline)
(display (realize one))
(newline)
(display (realize two))
(newline)

(define three (lambda (f) ((add-1 two) f)))

; my first naive attempt to implement addition actually turned out to be multiplication
(define (mul a b)
  (lambda (f) (lambda (x) ((b (a f)) x))))

; we first apply (a f) to x and then apply (b f) to the result
; this is in contrast to multiplication where we combine a and b before application to x.
(define (add a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

(display (realize (add two three)))
(newline)

(display (realize (mul two three)))
(newline)
