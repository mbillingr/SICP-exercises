(import (builtin core)
        (sicp utils))

(define (integrate f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((= k 0) (y k))
          ((= k n) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (sum term 0 inc n)
     (/ h 3)))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result
                          (term a)))))
  (iter a 0))

(display (integrate cube 0 1 100))
(newline)
(display (integrate cube 0 1 1000))
(newline)
