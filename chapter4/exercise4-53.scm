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

    (define (prime-sum-pair list1 list2)
      (let ((a (an-element-of list1))
            (b (an-element-of list2)))
        (require (prime? (+ a b)))
        (list a b)))

    (let ((pairs '()))
      (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
                 (permanent-set! pairs (cons p pairs))
                 (amb))
               pairs)))

  the-global-environment)

(driver-loop)
