(import (builtin core)
        (sicp utils))

(define (reverse lst)
  (define (iter in out)
    (if (null? in)
        out
        (iter (cdr in)
              (cons (car in) out))))
  (iter lst nil))

; recursive implementation
(define (fringe1 tree)
  (cond ((null? tree) nil)
        ((pair? tree) (append (fringe (car tree))
                              (fringe (cdr tree))))
        (else (list tree))))

; reversed partially iterative implementation
(define (fringe2 tree)
  (define (iter tree out)
    (cond ((null? tree) out)
          ((pair? tree) (iter (cdr tree)
                              (iter (car tree) out)))
          (else (cons tree out))))
  (reverse (iter tree nil)))

(define fringe fringe2)

(define x (list (list 1 2) (list 3 4)))
(display (fringe x)) (newline)
(display (fringe (list x x))) (newline)

(display "\nperformance on tiny tree\n")
(display "  recursive: ")
(timeit (lambda () (fringe1 x)))
(display "  iterative: ")
(timeit (lambda () (fringe2 x)))


(define a (list 1 2 3 4 5 6 7 8 9 10))
(define b (list a a a a a a a a a a))
(define c (list b b b b b b b b b b))

(display "\nperformance on huge tree\n")
(display "  recursive: ")
(timeit (lambda () (fringe1 c)))
(display "  iterative: ")
(timeit (lambda () (fringe2 c)))
