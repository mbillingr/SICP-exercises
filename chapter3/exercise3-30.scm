(import (builtin core)
        (sicp utils))

(define (ripple-carry-adder A B S c)
  (if (null? A)
      'ok
      (let ((c-in (make-wire)))
        (full-adder (car A) (car B) c-in (car S) c)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))))
