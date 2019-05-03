(import (builtin core)
        (sicp utils))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(println z1)
(set-to-wow! z1)
(println z1)

(println z2)
(set-to-wow! z2)
(println z2)
