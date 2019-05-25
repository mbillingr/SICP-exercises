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

    (define (even? x)
      (= (remainder x 2) 0))

    (if-fail (let ((x (an-element-of '(1 3 5))))
              (require (even? x))
              x)
            'all-odd))

  the-global-environment)

(driver-loop)
