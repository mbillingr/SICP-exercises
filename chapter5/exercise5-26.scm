
; copy-paste into evaluator
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;  n | #push | max-depth
; ---+-------+------------
;  0 |    29 |   8
;  1 |    64 |  10
;  2 |    99 |  10
;  3 |   134 |  10
;  4 |   169 |  10
;  5 |   204 |  10

;  y = a * x + b
; 64 = a * 1 + b
; 99 = a * 2 + b

; n_pushes = 35 * n + 29 
