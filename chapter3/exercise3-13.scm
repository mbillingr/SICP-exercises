(import (builtin core)
        (sicp utils))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; the interpreter is not yet equipped to deal with infinite lists..
;; try printing z for instance :)
