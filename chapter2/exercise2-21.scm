(import (builtin core)
        (sicp utils))

(define numbers (list 1 2 3 4 5 6))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      nil
      (cons (sqr (car items))
            (square-list (cdr items)))))

(display (square-list numbers))
(newline)

(define (square-list items)
  (map sqr items))

(display (square-list numbers))
(newline)
