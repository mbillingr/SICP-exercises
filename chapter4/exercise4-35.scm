(import (builtin core)
        (sicp utils))

(define (require p)
  (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

; some alternative solutions:

(define (an-integer-between lo hi)
  (let ((n (an-integer-starting-from lo)))
    (require (<= n hi))
    n))

(define (an-integer-between lo hi)
  (require (<= lo hi))
  (amb lo (an-integer-between (+ lo 1) hi)))

(define (an-integer-between lo hi)
  (amb lo
       (if (>= lo hi)
           (amb)
           (an-integer-between (+ lo 1) hi))))
