(import (builtin core)
        (sicp utils))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(println (cdr x))  ; expected (b)

(define w (append! x y))

(println (cdr x))  ; expected (b c d)
