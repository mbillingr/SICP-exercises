(import (builtin core)
        (sicp utils))

(include "chapter4-amb.scm")

(eval '(begin
         (define (require p)
           (if (not p) (amb)))

         (define (an-integer-starting-from n)
           (amb n (an-integer-starting-from (+ n 1))))

         (define (an-integer-between lo hi)
           (require (<= lo hi))
           (amb lo (an-integer-between (+ lo 1) hi)))

         ; can't test yet, so no attempt to optimize...
         (define (a-pythagorean-triple)
           (let ((k (an-integer-starting-from 1)))
             (let ((j (an-integer-between 1 k)))
               (let ((i (an-integer-between 1 j)))
                 (require (= (+ (* i i) (* j j)) (* k k)))
                 (list i j k))))))
      the-global-environment)

(driver-loop)
