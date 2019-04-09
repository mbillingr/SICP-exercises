#!/usr/bin/env -S guile -s
!#

(define (reverse lst)
  (define (iter in out)
    (if (null? in)
        out
        (iter (cdr in)
              (cons (car in) out))))
  (iter lst nil))

(define (deep-reverse lst)
  (define (reverse-car in)
    (if (pair? in)
        (iter in nil)
        in))
  (define (iter in out)
    (if (null? in)
        out
        (iter (cdr in)
              (cons (reverse-car (car in))
                    out))))
  (iter lst nil))

(define x (list (list 1 2) (list 3 4)))
(display x) (newline)
(display (reverse x)) (newline)
(display (deep-reverse x)) (newline)
