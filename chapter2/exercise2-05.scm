(import (builtin core)
        (sicp utils))

(define (cons a b)
  (* (ipow 2 a)
     (ipow 3 b)))

(define (car z)
  (factor 2 z))

(define (cdr z)
  (factor 3 z))


(define (ipow x n)
  (define (iter i r)
    (if (= i 0)
        r
        (iter (- i 1) (* x r))))
  (iter n 1))


(define (factor b x)
  (define (iter r n)
    (if (and (> r 0)
             (= (remainder r b) 0))
        (iter (/ r b) (+ n 1))
        n))
  (iter x 0))

(display (car (cons 9 7)))
(newline)
(display (cdr (cons 9 7)))
(newline)
