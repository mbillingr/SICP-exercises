(import (builtin core)
        (sicp utils))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(display (reverse (list 1 2 3 4 5 6 7 8 9)))
(newline)

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(display (reverse (list 1 2 3 4 5 6 7 8 9)))
(newline)
