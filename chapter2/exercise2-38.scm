(import (builtin core)
        (sicp utils))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (let ((r (accumulate op initial (cdr sequence))))
        (display (car sequence))
        (display " / ")
        (display r)
        (newline)
        (op (car sequence)
            r))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; expected result: 1/6
(display (fold-right / 1 (list 1 2 3)))
(newline)
; oops! actual result: 1.5
; why? because we start from the right and divide by the previous result: 3/1, 2/3, 1/(2/3)

; expected result: 1/6 - because we divide the previous result by each item starting from the left: 1/1, 1/2, 2/3
(display (fold-left / 1 (list 1 2 3)))
(newline)

; expected result: (1 (2 (3 ()))) - we do (list 3 ()), (list 2 (list 3 ())), ...
(display (fold-right list nil (list 1 2 3)))
(newline)

; expected result: (((() 1) 2) 3) - we do (list () 1), (list (list () 1) 2), ...
(display (fold-left list nil (list 1 2 3)))
(newline)


; I postulate that op must be commutative for fold-left and fold-right to produce the same result

(display (fold-right + 1 (list 1 2 3)))
(newline)
(display (fold-left + 1 (list 1 2 3)))
(newline)
