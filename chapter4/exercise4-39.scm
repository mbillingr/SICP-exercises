(import (builtin core)
        (sicp utils))

(include "chapter4-amb.scm")

; The order of the restrictions should certainly not affect the answer because
; a valid answer has to pass all restrictions anyway.
; The order might affect the performance to some degree. I think most of the
; time is lost in the backtracking framework itself, but putting the most
; restricticve requirements first might reduce the number of checks performed
; and improve performance slightly.

; Timing measurements show that the order does indeed have an impact on
; performance. However, it's not putting the most restrictive requirements first
; that matters but putting simple tests before complex tests.
; The procedure distinct? is a complex test. By putting it last we can recude
; the number of times it needs to be checked. (> miller cooper) is the simplest
; test with only one primitive call; it goes first. Sorting the tests roughly
; by the number of procedures called halved the overall runtime.

(eval '(define (distinct? items)
         (cond ((null? items) true)
               ((null? (cdr items)) true)
               ((member (car items) (cdr items)) false)
               (else (distinct? (cdr items)))))
      the-global-environment)

(eval '(define (member item x)
         (cond ((null? x) false)
               ((equal? item (car x)) x)
               (else (member item (cdr x)))))
      the-global-environment)

(eval '(define (multiple-dwelling)
         (let ((baker (amb 1 2 3 4 5))
               (cooper (amb 1 2 3 4 5))
               (fletcher (amb 1 2 3 4 5))
               (miller (amb 1 2 3 4 5))
               (smith (amb 1 2 3 4 5)))
           (require (> miller cooper))
           (require (not (= baker 5)))
           (require (not (= cooper 1)))
           (require (not (= fletcher 5)))
           (require (not (= fletcher 1)))
           (require (not (= (abs (- smith fletcher)) 1)))
           (require (not (= (abs (- fletcher cooper)) 1)))
           (require (distinct? (list baker cooper fletcher miller smith)))
           (list (list 'baker baker)
                 (list 'cooper cooper)
                 (list 'fletcher fletcher)
                 (list 'miller miller)
                 (list 'smith smith))))
      the-global-environment)

(timeit (lambda () (eval '(multiple-dwelling) the-global-environment)))

(driver-loop)
