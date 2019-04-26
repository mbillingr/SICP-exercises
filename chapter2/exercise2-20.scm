(import (builtin core)
        (sicp utils))

(define (same-parity first . rest)
  (define (iter in ref)
    (cond ((null? in) (list))
          ((= (even? (car in))
              ref)
           (cons (car in)
                 (iter (cdr in) ref)))
          (else (iter (cdr in) ref))))
  (cons first
        (iter rest (even? first))))

(display (list 1 2 3 4 5 6 7 8)) (display " -> ") (display (same-parity 1 2 3 4 5 6 7 8)) (newline)
(display (list 2 3 4 5 6 7 8)) (display " -> ") (display (same-parity 2 3 4 5 6 7 8)) (newline)
