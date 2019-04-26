(import (builtin core)
        (sicp utils))
        
(define (reverse lst)
  (define (iter in out)
    (if (null? in)
        out
        (iter (cdr in)
              (cons (car in) out))))
  (iter lst (list)))

(define a (list 1 2 3 4 5))
(define b (list 2 3 5 7 11))
(define c (list))

(display a) (display " -> ") (display (reverse a)) (newline)
(display b) (display " -> ") (display (reverse b)) (newline)
(display c) (display " -> ") (display (reverse c)) (newline)
