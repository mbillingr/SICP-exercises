(import (builtin core)
        (sicp utils))

(define (double f)
  (lambda (x) (f (f x))))

(display (((double (double double)) inc) 5))
(newline)
