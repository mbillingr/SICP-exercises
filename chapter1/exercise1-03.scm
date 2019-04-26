(import (builtin core))

(define (sos-max-pair a b c)
 (cond ((smallest? a b c) (sos-pair b c))
       ((smallest? b a c) (sos-pair a c))
       ((smallest? c a b) (sos-pair a b))))

(define (smallest? a b c) (and (<= a b) (<= a c)))

(define (<= a b) (not (> a b)))

(define (sos-pair x y) (+ (sqr x) (sqr y)))

(define (sqr x) (* x x))

(display (sos-max-pair 1 2 3)) (newline)
(display (sos-max-pair 2 3 1)) (newline)
(display (sos-max-pair 3 2 1)) (newline)
