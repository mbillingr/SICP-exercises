(import (builtin core)
        (sicp utils))

(include "chapter4-amb.scm")

(eval '(define (an-integer-between lo hi)
         (amb lo
              (if (>= lo hi)
                  (amb)
                  (an-integer-between (+ lo 1) hi))))
      the-global-environment)

(eval '(define (n-queens n)
        (define (make-queen row) (cons row (amb (an-integer-between 1 n))))
        (define (safe? queen others)
          (cond ((null? others) true)
                ((reachable? queen (car others)) false)
                (else (safe? queen (cdr others)))))
        (define (reachable? pos1 pos2)
          (let ((r1 (car pos1))
                (c1 (cdr pos1))
                (r2 (car pos2))
                (c2 (cdr pos2)))
            (cond ((= r1 r2) true)
                  ((= c1 c2) true)
                  ((= (abs (- r1 r2))
                      (abs (- c1 c2)))
                   true)
                  (else false))))
        (define (make-n n queens)
          (cond ((= n 0) queens)
                (else
                  (let ((new-queens (cons (make-queen n) queens)))
                    (require (safe? (car new-queens) queens))
                    (make-n (- n 1) new-queens)))))
        (make-n n '()))
      the-global-environment)

(all-solutions '(n-queens 8) the-global-environment)
(timeit (lambda () (eval '(n-queens 4) the-global-environment)))


(driver-loop)
