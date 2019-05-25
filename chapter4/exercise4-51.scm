(import (builtin core)
        (sicp utils))

(include "chapter4-amb.scm")

(all-solutions
  '(begin
    (define (require p)
      (if (not p) (amb)))

    (define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items))))

    (define count 0)

    (let ((x (an-element-of '(a b c)))
          (y (an-element-of '(a b c))))
      (permanent-set! count (+ count 1))
      (require (not (eq? x y)))
      (list x y count)))
  the-global-environment)

(driver-loop)

; without set! instead of permanent-set! the count would always be 1 because
; every set! operation is undone.
